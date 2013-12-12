-- Managin cuncurrent treads from RWH Chapter 24
module NiceFork (
      ThreadManager
    , newManager
    , ThreadStatus (Running,Finished,Threw)
    , forkManaged,forkManagedFinally --,forkManagedGTK
    , getStatus,myforkFinally'
    , waitFor
    , waitAll
    ,scrollBarInWindow
    ) where


import Control.Concurrent --(forkIO, MVar, putMVar,
                          -- takeMVar, newMVar, newEmptyMVar,tryTakeMVar)
import Control.Exception.Base
import Control.Exception (SomeException, try,block, catch, throw, unblock)
import Control.Monad.Trans (liftIO)


import qualified Graphics.UI.Gtk as G
import qualified Data.Map as M
import Prelude hiding (catch) 
import MyTime
import System.Random
import Data.List
import Data.IORef
import Text.Printf

data ThreadStatus = Running
                  | Finished         -- terminated normally
                  | Threw SomeException  -- killed by uncaught exception
                    deriving ( Show)

{--
    For the implementation of ThreadManager, we maintain a map from thread
    ID to thread state. We'll refer to this as the thread map
--}
newtype ThreadManager =
    Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
    deriving (Eq)


-- | Create a new thread manager.
newManager :: IO ThreadManager
newManager = Mgr `fmap` newMVar M.empty

{-- | Create a new managed thread.
For each thread that we manage, we maintain an MVar. A per-thread MVar starts
off empty, which indicates that the thread is executing. When the thread
finishes or is killed by an uncaught exception, we put this information into
the MVar.

To create a thread and watch its status, we must perform a little bit of book-keeping.
--}
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
    modifyMVar mgr $ \m -> do
      state <- newEmptyMVar
      tid <- forkIO $ do
        result <- try body
        putMVar state (either Threw (const Finished) result)
      return (M.insert tid state m, tid)

{--   | Immediately return the status of a managed thread.
getStatus function tells us the current state of a thread. If the thread is
no longer managed (or was never managed in the first place), it returns Nothing.
If the thread is still running, it returns Just Running. Otherwise, it
indicates why the thread terminated, and stops managing the thread.
--}
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m, Nothing)
      Just st -> tryTakeMVar st >>= \mst -> case mst of
                   Nothing -> return (m, Just Running)
                   Just Running -> return (m, Just Running)
                   Just sth -> return (M.delete tid m, Just sth)

-----------------------------------------------------------------------------
forkManagedFinally :: ThreadManager -> IO () -> (Either SomeException () -> IO ()) -> IO ThreadId
forkManagedFinally (Mgr mgr) body and_then =
    modifyMVar mgr $ \m -> do
      state <- newEmptyMVar
      tid <- myforkFinally (do
        result <- try body
        putMVar state (either Threw (const Finished) result))  and_then
      return (M.insert tid state m, tid)
    --
-- forkFinally
myforkFinally action and_then =
   mask $ \restore ->
      forkIO $ try (restore action) >>= and_then

myforkFinally'  :: Exception e => IO () -> (Either e () -> IO ()) -> IO ThreadId
myforkFinally' io f = myforkFinally io f
--}

{-- | Block until a specific managed thread terminates.
The waitFor function behaves similar to getStatus, but instead of returning
immediately, it blocks until the given thread terminates before returning
--}
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = do
  maybeDone <- modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, Nothing)
      (done, m') -> (m', done)
  case maybeDone of
    Nothing -> return Nothing
    Just st -> Just `fmap` takeMVar st

{-- | Block until all managed threads terminate.
The waitAll function simply waits for all currently managed threads
to complete, and ignores their exit statuses.
--}
waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
    where elems m = return (M.empty, M.elems m)


------------------ testing the Mvar
main = do
    g <- getStdGen
    forkRef <- newIORef False
    let random_list = (randomRs (0,55555) g):: [Double]
    putStrLn "Enter the number of numbers to sort: "
    input <- getLine
    let n =  read input
    let list = take n random_list
    thredMan <- newManager
    thredId <- forkManaged thredMan $ do
                   let slist = sort list
                   print $ take 20 slist
                   writeIORef forkRef True
                   return ()
    thredStatus <- getStatus thredMan thredId
    case thredStatus of
       Just Running -> do
            --printf "%s %s\n" "The first 10 numbers are: " (show $ take 10 list)
            printf "\n %s %d %s \n" "Now sorting the " n " Numbers. Please wait..."
            keepChecking forkRef $ do
                putStrLn "Finished sorting... Press any key to continue..."
                a <- getLine
                return ()
            {--
            thredWstatus <- waitFor thredMan thredId
            case thredWstatus of
                _ -> return () --}
       Just (Threw _) -> putStrLn "there was an exception"
       Nothing  -> putStrLn "thred finished or  was never managed"
       Just Finished -> putStrLn "Thread finished"


keepChecking :: IORef Bool -> IO() -> IO()
keepChecking   trefRef  io = do
    flag <- readIORef trefRef
    if flag then io else keepChecking trefRef io
{--
modifyMVar takes the value from an MVar, and passes it to a function. This function
can both generate a new value and return a result. If the function throws an exception,
modifyMVar puts the original value back into the MVar, otherwise it puts the new value in.
It returns the other element of the function as its own result. No comments

When we use modifyMVar instead of manually managing an MVar with takeMVar and putMVar,
we avoid two common kinds of concurrency bug. Forgetting to put a value back into an MVar.
This can result in deadlock, in which some thread waits forever on an MVar that will never
have a value put into it. No comments

Failure to account for the possibility that an exception might be thrown, disrupting
the flow of a piece of code. This can result in a call to putMVar that should occur
not actually happening, again leading to deadlock. No comments

Because of these nice safety properties, it's wise to use modifyMVar whenever possible

-- }
modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m io =
  block $ do
    a <- takeMVar m
    (b,r) <- unblock (io a) `catch` \e -> putMVar m a >> throw e
    putMVar m b
    return r --}

--- put a scroll bar in a window
scrollBarInWindow :: G.Window -> Maybe G.Label -> G.ProgressBar -> ThreadId -> String -> IO ()
scrollBarInWindow win mdes pr tId labelInfo = do
    upBox <-   G.vBoxNew False 1
    actionArea <- G.hBoxNew False 2
    --
    G.set win [G.containerChild G.:= upBox]
    ---- get DCC image
    G.hBoxNew False 1 >>= \hbox -> do
        image <-  G.imageNewFromFile  "./Logos/DCC_logo_small.png" -- (-1) 100
        G.set hbox [G.containerChild G.:= image]
        G.boxPackStart upBox hbox G.PackNatural 3
    maybe (return ()) (\lb -> G.boxPackStart upBox lb G.PackNatural 6) mdes
    -- pack the scroll bar
    G.hBoxNew True 1 >>= \prBox -> do
        G.boxPackStart prBox pr G.PackGrow 2
        G.boxPackStart upBox prBox G.PackNatural 5
    --
    cancelButt <- G.buttonNewWithLabel "  Cancel  "
    G.boxPackEnd actionArea cancelButt G.PackNatural 2
    G.set win [G.windowTitle G.:= (labelInfo ++"...")]
    --- handle for cancel
    G.on cancelButt G.buttonActivated $ do
        killThread tId -- kill the thread
        G.widgetDestroy win  -- destroy the window
        print "Thread stopped"
    ---
    sep <- G.hSeparatorNew
    G.boxPackStart upBox sep G.PackNatural 1
    G.boxPackStart upBox actionArea G.PackNatural 2
    --
