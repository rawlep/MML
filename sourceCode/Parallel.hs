-----------------------------------------------------------------------------
--
-- Module      :  Parallel
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Parallel (forkProcess
    ,openProgessWindow
    ,forkProcessWithID
    ,forkProcessWithScroll
    ,progressDialogWithTrackListAndInfoButt
    ,forkProcessWithScrollTexts
    ,processWithScroll
    ,myTry
    ) where

import qualified Graphics.UI.Gtk as G
import qualified Data.ByteString.Lazy.Char8 as L (unpack,dropWhile,readFile,writeFile,pack)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM) -- , liftM2)
import Control.Concurrent (forkIO,killThread,ThreadId)
import Control.Exception as E (SomeException,catch,IOException)
--import System.Directory (doesFileExist)
import Data.Maybe (isJust,maybe,listToMaybe)
import Data.List (delete, find, deleteBy,maximumBy, nub, sort)
import Data.Char (isDigit)
import Data.Ord (comparing)
import GlobalParameters --(EnabledLists,modifyNextChainClick)
import System.Directory (doesFileExist) -- , doesDirectoryExist,createDirectory)
import Data.IORef
---------------------------------------------------------------------------------------------------
-- graphing
import Numeric.LinearAlgebra hiding (find)
--import Graphics.UI.Gtk hiding(Circle,Cross)
import Control.Monad.Trans
--import Graphics.Rendering.Plot -- as P
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart.Gtk

import Numeric.GSL
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------
import GlobalParameters --(EnabledLists,modifyNextChainClick)
import NiceFork
--import QueryDialog (runQueryDialog )
--import ReadFromDataFiles1 (bs2HMDIF2bs1 )
import ProcessFile (display,log2File)
import HMDIFPrelude (Defect (..) ,scode2align , typesAndDetails,getTypeFromDetail,conditions,nthComma')
--import ReadFromDataFiles1 (readRawOutput,bs2HMDIF2bs_aux)
import RprMix2 (oMod,alph, sigD, sigE)
import ListStats (mean)


--threadFinished

test = forkProcess Nothing (\_ -> return ()) printFact (Just (print "fininshed"))
    where
        printFact = do
            let k = sort ([1 .. 500] ++ [700 .. 100])
            print (show $ (k !! 200))

--- forking processes with messages
forkProcess ::  Maybe String -> -- description of the process
    (String -> IO ()) -> -- function to print exceptions and process steps somewhere
    IO () ->  -- the process to fork
    Maybe (IO ()) ->  -- possible thing to do when the process ends
    IO ()
forkProcess mdes writer process endAction = do
     ----------------------- the threadding --------------------------------------
    thredMan <- newManager --
    thredId <- forkManagedFinally thredMan process (\ _ -> do
                                                         maybe (return ()) id endAction -- >
                                                         maybe (return ()) (writer . ("Finished: " ++)) mdes )
    thredStatus <- getStatus  thredMan thredId  -- getStatus
    case thredStatus of
        Just (Threw e) -> do
               let exceptionMessage = ("Exception : " ++ show (e :: SomeException))
               -- G.postGUIAsync $ runMsgDialog (Just "Exception") exceptionMessage Error
               writer ("Exception : " ++ show (e :: SomeException))
        Just _  -> return () -- writer ("Finished: " ++ description)
    --


---- same as fork process, but returns the thread ID
forkProcessWithID  mdes writer process endAction = do -- mdes writer
     ----------------------- the threadding --------------------------------------
    thredMan <- newManager --
    thredId <- forkManagedFinally thredMan process (\ _ -> endExecution endAction mdes )
    thredStatus <- getStatus  thredMan thredId  -- getStatus
    case thredStatus of
        Just (Threw e) -> do
               let exceptionMessage = ("Exception : " ++ show (e :: SomeException))
               G.postGUIAsync $ runMsgDialog (Just "Exception") exceptionMessage Error
               --
             --G.postGUIAsync $ writer ("Exception : " ++ show (e :: SomeException))
        Just _  -> return ()
             -- maybe (return ()) (writer . ("Finished: " ++)) mdes
    return thredId
        where
            endExecution endAction mdes = do
                maybe (return ()) id endAction
                maybe (return ()) (writer . ("Finished: " ++)) mdes



-----
forkProcessWithScrollTexts :: String -> IO () -> Maybe (IO a) -> IO ()
forkProcessWithScrollTexts wTitle processToFork possibleEndProcess = do
    label <- G.labelNew (Just wTitle)
    let writer = G.labelSetText label
    forkProcessWithScroll (Just label) writer processToFork possibleEndProcess

forkProcessWithScroll ::  Maybe G.Label -> (String -> IO ()) -> IO () -> Maybe (IO a) -> IO ()
forkProcessWithScroll mdes writer process endAction = do
    win <- G.windowNew
    -- let title = maybe "Processing" id winTitle
    G.postGUIAsync $ G.set win [G.windowResizable G.:= True, G.windowModal G.:= False,
                                G.windowUrgencyHint G.:= True]
    G.postGUIAsync $ G.widgetSetSizeRequest win 400 (-1) -- 200
    let destWin = G.widgetDestroy win
    let newEndAction = maybe (Just destWin) (\act -> Just (act >> destWin)) endAction
    mdes' <-  case mdes of
                Nothing -> return Nothing
                Just lbl ->  do
                        str <- G.labelGetText lbl
                        return (Just str)
    --- progress bar ----------------
    pr <- G.progressBarNew
    G.progressBarSetPulseStep pr 0.25
    --
    tId <- forkProcessWithID mdes' writer process newEndAction
    scrollBarInWindow' win mdes pr tId (maybe "" id mdes') -- ) >> return ()
    G.on win G.mapEvent $ liftIO $ do
        G.timeoutAdd (showPulse pr) 700
        return True
    G.widgetShowAll win
    where
        -------
        showPulse :: G.ProgressBar -> IO Bool
        showPulse b = do G.progressBarPulse b
                         return True
        ----------
        --scrollBarInWindow' :: G.Window -> Maybe G.Label -> G.ProgressBar -> ThreadId -> Maybe String -> IO ()
        scrollBarInWindow' win mdes pr tId labelInfo = do
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

            ---
            G.hSeparatorNew >>= \sep ->
                    G.boxPackStart upBox sep G.PackNatural 1
            G.boxPackStart upBox actionArea G.PackNatural 2
            -- return ()
            --G.widgetShowAll win
            --}


-----------------------------------------------------------------------------------}
--                       Progress Bar
-----------------------------------------------------------------------------------
-- | a less general progress window with just a description
-- | of the process and the process itself
openProgessWindow :: String  -> String -> IO () -> IO G.Window
openProgessWindow windowTitle description process = do
    let f _ = return ()
    progressDialogWithTrackListAndInfoButt windowTitle description f Nothing Nothing process

-- | a general progress dialog. This is suitable for when
-- | each steps in the process are to be tracked. If such
-- | is the case, the function is called with a text buffer
-- | in the 5ht argument along with a function to update
-- | the text buffer (and any other logging requirements)
-- | in its third argument
------------------------------- have to rewrite this
progressDialogWithTrackListAndInfoButt :: String -> -- the window title
    String -> -- the content of the info label
    (String -> IO ()) -> -- function to print exceptions and process steps somewhere
    Maybe String -> -- possible string for the info button
    Maybe (Either (G.Label,G.TextBuffer) G.TextBuffer) -> -- possible entry for the track list
    IO () ->  -- the process to fork
    --(Maybe (IO ())) ->  -- thing to do when the process ends
    IO (G.Window)
progressDialogWithTrackListAndInfoButt wTitle lInfo prt idetails buff pcess  = do
    G.windowNew >>= \win -> do
        let !width = 12 * (length lInfo)
        let !height = width `div` 2
        --G.widgetSetSizeRequest win width height
        G.set win [G.windowResizable G.:= False,G.windowModal G.:= False,
                   G.windowUrgencyHint G.:= True] --G.windowAllowGrow G.:= True
        G.hBoxNew False 1 >>= \mvbox ->  do
            vbox <- progDialogWithTrackListAndInfoButt_aux win wTitle lInfo prt idetails buff pcess
            G.boxPackStart mvbox vbox G.PackGrow 7
            G.widgetShowAll mvbox
            G.set win [G.containerChild G.:= mvbox]
            --
        return win


-- the upper and lower boxes
progDialogWithTrackListAndInfoButt_aux dia windowTitle labelInfo prt infoDetails inBuffer process = do
    upBox <-   G.vBoxNew False 1
    actionArea <- G.hBoxNew False 2
    --
    cancelButt <- G.buttonNewWithLabel "  Cancel  "
    G.boxPackEnd actionArea cancelButt G.PackNatural 2
    G.set dia [G.windowTitle G.:= (labelInfo ++"...")]
    --- progress bar ----------------
    pr <- G.progressBarNew
    G.progressBarSetPulseStep pr 0.25
    -- add the options
    case maybe2 infoDetails inBuffer of
        Just (info,tbufOrlabel) -> do
            infoButt <- G.buttonNewWithLabel " Check Details "
            G.on infoButt G.buttonActivated $ runMsgDialog (Just "Working") info Info
            G.boxPackStart actionArea infoButt G.PackNatural 2
            --labelbox <- labelLeftofWidgetE labelInfo G.JustifyCenter infoButt True
            --labelbox <- G.labelNew (Just labelInfo)
            -------------------------------------------------------------------------
            G.hBoxNew False 1 >>= \hbox -> do
                image <-  G.imageNewFromFile  "./Logos/DCC_logo_small.png" -- (-1) 100
                G.set hbox [G.containerChild G.:= image]
                G.boxPackStart upBox hbox G.PackNatural 3
            ---
            G.hSeparatorNew >>= \sep ->
                G.boxPackStart upBox sep G.PackNatural 0
            case tbufOrlabel of
                Left (label,tbuff) -> do
                    G.boxPackStart upBox label G.PackNatural 4
                    --G.hBoxNew True 1 >>= \prBox -> do
                    --    G.boxPackStart prBox pr G.PackGrow 2
                    --     G.boxPackStart upBox prBox G.PackNatural 4
                    --
                    labelLeftofWidgetE "Completed parameters" 20 infoButt True >>= \labl ->
                        G.boxPackStart upBox labl G.PackNatural 1
                    --
                    (scrwin , _) <- makeScrolledEntryArea VScroll False True
                    -- makeScrolledEntryArea tbuff False >>= \scrwin ->
                    G.boxPackStart upBox scrwin G.PackGrow 1
                    --
                Right tbuff -> do
                    --G.hBoxNew True 1 >>= \prBox -> do
                    --    G.boxPackStart prBox pr G.PackGrow 2
                    --    G.boxPackStart upBox prBox G.PackNatural 4
                    --
                    makeScrolledEntryArea VScroll False True >>= \(scrwin,_) ->
                    --makeScrolledEntryArea tbuff False >>= \scrwin ->
                        G.boxPackStart upBox scrwin G.PackGrow 1
        Nothing ->  do
            labl <- G.labelNew (Just labelInfo)
            G.boxPackStart upBox labl G.PackNatural 6
            --
            G.hBoxNew True 2 >>= \prBox -> do
                G.boxPackStart prBox pr G.PackGrow 2
                G.boxPackStart upBox prBox G.PackNatural 4
            --G.boxPackStart upBox pr G.PackNatural 4
    ----------------------- the threadding --------------------------------------
    forkProcess Nothing prt process (Just $ endAction dia)
    G.on dia G.mapEvent $ liftIO $ do
        G.timeoutAdd (showPulse pr) 700 -- >>
        return True

    G.on cancelButt G.buttonActivated $ do
        endAction dia -- G.widgetDestroy dia
    ------------------------ the action area -------------------------------------
    G.hSeparatorNew >>= \sep ->
                    G.boxPackStart upBox sep G.PackNatural 1
    G.boxPackStart upBox actionArea G.PackNatural 2
    return upBox
    where
        showPulse :: G.ProgressBar -> IO Bool
        showPulse b = do G.progressBarPulse b
                         return True
        endAction win = do
            G.widgetDestroy win
------------------------------------------------------------------------------------

-- process with scroll
processWithScroll :: Maybe MyDialog ->
        Maybe String ->  --- description of the process
        G.Label ->  --- the label for the updating processes Maybe
        (String ->  IO ()) -> -- the writing function
        IO () ->    -- the process to fork
        IO ()
processWithScroll mWin mDiscrpt srlLabel wrt process = do
    -- remove any file that exits
    L.writeFile "./executionError.txt" (L.pack "")
    --- progress bar ----------------
    pr <- G.progressBarNew
    G.progressBarSetPulseStep pr 0.25
    ----------------------------------wrt
    win <- maybe myDialogNew return mWin
    G.postGUIAsync $ G.widgetSetSizeRequest (myDia win) 420 (-1) -- 180
    modifyIORef (title  win) (\s -> maybe s id mDiscrpt)
    ----
    G.on (myDia win) G.mapEvent $ liftIO $ do
          G.timeoutAdd (showPulse pr) 700
          return True
    --- action for the end of the process
    let endAction =  (G.postGUIAsync $ do
                           G.widgetDestroy (myDia win)
                           exist <- doesFileExist "./executionError.txt"
                           let endNOrmally = do
                                                currMessage <- readIORef (title win)
                                                runMsgDialog  (Just "Process Completed") ("Finished: " ++ currMessage) Info
                           if exist then do
                                message <- liftM L.unpack $ L.readFile "./executionError.txt"
                                if message /= "" then do
                                    modifyIORef (title win) (\_ -> message)
                                    runMsgDialog  (Just "Process Aborted") message Info
                                else
                                    endNOrmally
                           else
                               endNOrmally)
    --
    forkProcessWithID mDiscrpt wrt process (Just endAction) >>= showScrollWindow  win pr srlLabel endAction -- abortAction
    where
        ---
        showScrollWindow ::
            MyDialog  ->
            G.ProgressBar ->
            G.Label -> -- Maybe wrt
            IO () ->
            -- IO () ->abortAction
            ThreadId ->
            IO ()
        showScrollWindow win pr mLabel endAction  threadId = do
            G.onDestroy (myDia win) $ do
                killThread threadId
                return ()
                --tss <- readIORef $ title win
                -- if nll tss then
                --endAction -- (myDia win) threadId
            upBox <- myDialogGetUpper win
            --
            G.hBoxNew False 1 >>= \hbox -> do
                image <-  G.imageNewFromFile  "./Logos/DCC_logo_small.png" -- (-1) 100
                G.set hbox [G.containerChild G.:= image]
                G.boxPackStart upBox hbox G.PackNatural 0
            --
            G.boxPackStart upBox mLabel G.PackGrow 3
            --maybe (return ())
                  --(\lab -> )
            --      mLabel
            G.boxPackStart upBox pr G.PackNatural 5
            --- bottom of the dialon
            --- adding the cancel button
            cancelButton <- G.buttonNewWithMnemonic "_Cancel"
            --lower <- G.dialogGetActionArea win
            myDialogAddButton win cancelButton
            G.on cancelButton G.buttonActivated $ do
                modifyIORef (title  win) ("Aborted: "++ )
                killThread threadId
                --abortAction -- (myDia win) threadId
            --
            myDialogShow win
        ---
        showPulse :: G.ProgressBar -> IO Bool
        showPulse b = do G.progressBarPulse b
                         return True
        ---
        --endAction win tId = do
        --    killThread tId
        --    G.widgetDestroy win

--}
--------------------------------------------------------------------------
myTry :: IO () -> IO ()
myTry  process = do
    let write2log str = runMsgDialog Nothing str Error >> log2File "logfile1" str
    E.catch process (\e -> write2log $ show (e :: E.IOException ))
