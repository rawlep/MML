{-# LANGUAGE BangPatterns #-}

module GlobalParameters (DialogType (Warning,Question,Info,Error)
                    , TScroll (SNone, HScroll , VScroll , BScroll)
                    --AddedCombos (Rut,Tex,Profl,Crk) --SMaybe (SNothing, SJust)
                    ,MyDialog (..), myDialogNew,myDialogAddButton,myDialogShow
                    ,myDialogGetUpper,myDialogGetLower,myDialogAddWidget
                    ----
                    ,widgetInWindow
                    ,vScrolledWinWithWgtList, hPanedArea, vPanedArea, notebookInFrame
                    ,makeScrolledEntryArea,notebookWithScrollWin,horizontalWidgetList
                    ,widgetLeftWifget, noteBookAddWidget,processDialogNew
                    ---------------------------------- utilities for combos, etc ------------------
                    ,fillToMaxLegth,fillToMaxLegthN,setWidth,mWhenGen,mWhen
                    ,clearComboList,addToComboList,clearAdd,update
                    ------------------------------------------------------------------------
                    ,tableWithWidgetList,runMsgDialog,toggleEntyOnFocusOut,toggleEntyOnFocusOutGen
                    --,newWarningMgs,newErrMgsWin,runWarningMessageWindow,runQuestionMessage, runInfoMessage,runErrorMessage
                    ,toggleMessageifChange,remSp,actionsOnToggle
                    ,runDoubleDecisionMessageWindow, newMessageInfo
                    ,runDecisionMessageWindow,entVal,maybe3,maybe2,bring2Front,fst3,snd3,trd --,mOvl2mDf
                    ------------------------------------------------------------
                    ,checkBoxWithLeftLabel,comboBoxWithLeftLabel,makeRadioButtonGroupFromStrings
                    ,labelLeftofWidget,labelLeftofWidgetE,labelLeftofWidgetLabel,labelLeftofWidgetE'
                    -----------------------------------------------------------------
                    ,setAlignOptions,setRefOnComboOption -- ,setRefOnComboChange setAlignOptionsOnChange,
                    -----------------------------------------------------------------
                    ,DrawRef (..), emptyDrawRwf,updateDrawData,updateCurrChain
                    ,updateAddChain,mkDrawRef
                    ,updateCaption,updatePtsLabel,updateFunction,Results (..),RResults
                    ,removeFunction, removeResults, updateResults
                    -----------------------------------------------------------------------
                    , updateBufferAtEnd,clearTextBuffer
                    , myModifyIORef
                    , module ProcessFile
            ) where

--- imports ---------------------------------------------------------------------------------------
import qualified Graphics.UI.Gtk as G
--import Control.Monad.Trans (liftIO)
import Char (isDigit,isSpace)
--import RprMix (Mmodel)
import Data.List (find,findIndex,delete,deleteBy, insertBy, nubBy,sortBy, nub) --
import Data.Maybe (fromJust,listToMaybe,isJust)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM,liftM2)
import  Data.Ord (comparing)
import Data.IORef
import Graphics.Rendering.Plot (OutputType)
import LMisc (mkIntervals)
----
import ProcessFile


------------------------------------------------------------------------------------------------
-- SOME UNTILY FUNCTIONS
------------------------------------------------------------------------------------------------
-- fillToMaxLegth,fillToMaxLegthN,setWidth,mWhenGen,mWhen,clearComboList,addToComboList,clearAdd,update
--
fillToMaxLegth :: [String] -> [String]
fillToMaxLegth strings = fil_aux (maximum $ map length strings) strings
    where
        fil_aux :: Int -> [String] -> [String]
        fil_aux n    = map (\x -> (addSpace n $ length x) ++ x)
        addSpace m n = replicate (m - n) ' '

fillToMaxLegthN n = map ((replicate n ' ') ++) . fillToMaxLegth

setWidth :: Int -> String -> String
setWidth n str | len < n    =  str ++ replicate (n - len) ' '
               | otherwise  =  str
    where len = length str

--
mWhenGen :: Monad m => b -> (a -> m b) ->  Maybe a -> m b
mWhenGen  b f = maybe (return b) f

--
mWhen f = mWhenGen () f
--
-- update
-- clears the combo list
clearComboList :: IORef [(G.ComboBox, Int)] -> G.ComboBox -> IO ()
clearComboList cRef combo = do
    readIORef cRef >>= flushCombo combo
        where flushCombo  c clist = do
                case find ((== c) . fst) clist of
                   Nothing -> return ()
                   Just (_, n) -> sequence_ (replicate n (G.comboBoxRemoveText c 0))


-- adds to the combolist and retains the number of items added
addToComboList :: IORef[(G.ComboBox,Int)] -> G.ComboBox -> [String] -> IO ()
addToComboList cRef combo list = do
    mapM_ (G.comboBoxAppendText combo) list >>
        mWhen (\_ -> G.comboBoxSetActive combo 0) (listToMaybe list)
    modifyIORef cRef (nub . ((combo, length list):))

-- convenience functon to addAfter cleaaring
clearAdd cRef combo list = clearComboList cRef combo >> addToComboList cRef combo list


-- | convenience function which activates a string in a combobox if it
-- exists in the given list of strigs (usually the LOS in the combo box)
update cmb str list = mWhen (G.comboBoxSetActive cmb ) $ findIndex (== str) list
--

widgetInWindow :: G.WidgetClass w => w -> IO G.Window
widgetInWindow  widget = do
    win <- G.windowNew
    G.set win [G.windowTitle G.:= "Visualizations", G.windowResizable G.:= True,
               G.windowDefaultWidth G.:= 900, G.windowDefaultHeight G.:= 600]
    G.set win [G.containerChild G.:= widget]
    return win
--                      END OF UTILITY FUNCTIONS
---------------------------------------------------------------------------------------------------

--initDraw =  OneType $ DRAW [] Nothing (MPlot Nothing Nothing) (False, Nothing)
----------------------------------------------------------------------------------------------------
--          DRAWING ON THE CANVAS (ONLY FOR THE ANALYSIS WINDOW FOR THE TIME BEING)
{--data DrawingOptions = DD -}
{-
Results (alp, std , ste,abar ,tau , rpRpredictions , rprCutsFreq)
--}
--  results related to one section
data Results = Results {
    alp :: Double,
    std :: Double,
    ste :: Double,
    abar :: [Double],
    tau :: Double,
    rpRpredictions :: [Double],
    rprCutsFreq :: [Int]
    -- tdsCuts :: [Int]
    } deriving Show
type RResults = ([Results] ,[Int])

data DrawRef = DrawRef {function :: Maybe (Double -> Double)
                        ,drawData :: [[(Double,Double)]]
                        ,currChain :: Int
                        ,addChain :: Int
                        ,caption :: String
                        ,chnLabel :: [String]
                        ,rslts    :: Maybe Results
                        }

--- map (sortBy  (comparing fst))
--- [((String , [(String, [(Double,OBs)])]), RResults ) ]
mkDrawRef :: (String , [(String, [(Double,Double)])])  -> RResults ->  (String , [DrawRef])
mkDrawRef (sec_and_srg , dChnLs) rrslts = (sec_and_srg , zipWith mkDrwRef_aux grp (fst rrslts))
    where
        grp =  fst . unzip . mkIntervals dChnLs $ snd rrslts
        mkDrwRef_aux :: [(String, [(Double,Double)])]  -> Results ->  DrawRef
        mkDrwRef_aux dChns rslt = DrawRef Nothing dat 1 ln "" lables (Just rslt)
            where
                (lables,dat) = unzip $ map  srtSnd dChns
                ln           = length dat
                srtSnd (a,ks) =  (a, sortBy  (comparing fst) ks)


-- tableWithWidgetList :: G.WidgetClass widget => Bool -> [widget] -> Bool -> Int -> IO G.Table
-- tableWithWidgetList isVertical  widgets homo spacing
--}

-- INITIALIZE A DRAWREF
emptyDrawRwf = DrawRef Nothing [[(0,0)]] 0 1 "" [] Nothing

updateDrawData :: [[(Double,Double)]] -> DrawRef -> DrawRef
updateDrawData ddata dref = dref{drawData = map (map flr . sortBy (comparing fst)) ddata}
    where flr (a,b) = (fromIntegral $ floor a , b)

updateCurrChain :: Int -> DrawRef -> DrawRef
updateCurrChain n drf = drf{currChain = n}

updateAddChain :: Bool -> Int -> DrawRef -> DrawRef
updateAddChain add n drf | n <  0    = drf
                         | add       = addChains n drf
                         | otherwise = drf {addChain = xs} -- remChains n drf
    where
        addChains n drf  | n  <= lim = drf {addChain = n}
                         | otherwise = drf
        -- removing
        remChains n drf  | k  <= 1   = drf
                         | otherwise = drf {addChain = k}
        m   = addChain drf
        k   = m - n
        nm  = m + n
        xs  = (length (chnLabel drf))  -- - 1
        lim = length $ drop (currChain drf) (chnLabel drf)

updateCaption :: String -> DrawRef -> DrawRef
updateCaption str drf = drf{caption = str}

updatePtsLabel :: [String] -> DrawRef -> DrawRef
updatePtsLabel xs drf = drf{chnLabel = xs}

updateFunction :: (Double -> Double) -> DrawRef -> DrawRef
updateFunction f drf = drf{function = Just f}

removeFunction ::  DrawRef -> DrawRef
removeFunction drf = drf{function = Nothing}

removeResults :: DrawRef -> DrawRef
removeResults drf = drf{rslts = Nothing}

updateResults :: Results -> DrawRef -> DrawRef
updateResults rs drf = drf{rslts = Just rs}
-------------------------------------------------------------------------------------------------


--------------------------------------------------------------------------------------------------------------
---                      SOME AUXILLARY FUNCTIONS AND WINDOWS
newWarningMgs st = G.messageDialogNew Nothing [G.DialogDestroyWithParent] G.MessageWarning G.ButtonsYesNo st
newErrMgsWin st = G.messageDialogNew Nothing [G.DialogDestroyWithParent] G.MessageError G.ButtonsOk st
newMessageInfo st = G.messageDialogNew Nothing [G.DialogDestroyWithParent] G.MessageInfo G.ButtonsOk st
newMessageQuestion st = G.messageDialogNew Nothing [G.DialogDestroyWithParent] G.MessageQuestion G.ButtonsYesNo st
--newMessageOther st = G.messageDialogNew Nothing [G.DialogDestroyWithParent] G.MessageWarning G.ButtonsYesNo st

---------------------------------------------------------------------------
-- MyDialog (..), myDialogNew,myDialogAddButton,myDialogShow,myDialogGetUpper,myDialogGetLower,myDialogAddWidget
data MyDialog = MyDialog {
        myDia :: G.Window,
        upper :: G.VBox,
        lower :: G.HBox,
        title :: IORef String
    }
--- greating a my dialog
myDialogNew ::  IO MyDialog
myDialogNew = do
    win  <- G.windowNew
    title <- newIORef ""
    ---
    bottombox <- G.hBoxNew False 2
    topBox <- G.hBoxNew False 2
    mainBox <- G.vBoxNew False 0
    vbox <- G.vBoxNew False 2
    sep <- G.hSeparatorNew
    --
    G.set win [G.windowResizable G.:= True, G.containerChild G.:= mainBox,
               G.windowUrgencyHint G.:= True]
    ---
    G.boxPackStart topBox vbox G.PackGrow 2
    G.boxPackStart mainBox topBox G.PackGrow 2
    G.boxPackStart mainBox sep G.PackNatural 2
    G.boxPackEnd mainBox bottombox G.PackNatural 2
    return $ MyDialog win  vbox bottombox title

--
myDialogAddButton :: MyDialog -> G.Button -> IO ()
myDialogAddButton dia button = do
    G.widgetSetSizeRequest button (-1)  30
    G.boxPackEnd (lower dia) button G.PackNatural 4
---
myDialogAddWidget :: G.WidgetClass w => MyDialog -> w -> IO ()
myDialogAddWidget dia w =
    G.boxPackStart (upper dia) w G.PackNatural 2
----
myDialogShow :: MyDialog -> IO ()
myDialogShow mda = do
    str <- readIORef $ title mda
    G.set (myDia mda)  [G.windowTitle G.:= str]
    G.widgetShowAll (myDia mda)

---
myDialogGetUpper :: MyDialog ->  IO G.VBox
myDialogGetUpper  = return . upper

--
myDialogGetLower :: MyDialog ->  IO G.HBox
myDialogGetLower = return . lower
-----------------------------------------------------------------------------------


--MessageQuestion
runWarningMessageWindow mStr string = do
    dia <- newWarningMgs string
    maybe (return ()) (\str -> G.set dia [G.windowTitle G.:= str]) mStr
    ans <- G.dialogRun dia
    G.dialogResponse dia ans
    G.widgetDestroy dia

-- run ErrorMessage
runErrorMessage mStr string = do
    dia <- newErrMgsWin string
    maybe (return ()) (\str -> G.set dia [G.windowTitle G.:= str]) mStr
    ans <- G.dialogRun dia
    G.dialogResponse dia ans
    G.widgetDestroy dia

-- runInfoMessage
runInfoMessage mStr string = do
    dia <- newMessageInfo string
    maybe (return ()) (\str -> G.set dia [G.windowTitle G.:= str]) mStr
    ans <- G.dialogRun dia
    G.dialogResponse dia ans
    G.widgetDestroy dia

-- a decision message dialog: displays a message requesting whether or not to
-- execute a process. If Yes is clicked, the provess is executed. Otherwise
-- nothing is done
runQuestionMessageGen :: Maybe String -> String -> Maybe (IO ()) -> Maybe (IO()) -> IO()
runQuestionMessageGen mStr string mprocess1 mprocess2 = do
    let xs = [ x | x <- [mprocess1 , mprocess2], isJust x]
    dia <- newMessageQuestion string
    case maybe2 mprocess1 mprocess1 of
        Just (proc1, proc2) -> do
             let process1 = G.widgetDestroy dia >> proc1
             let process2 = G.widgetDestroy dia >> proc2
             ans <- G.dialogRun dia
             if ans == G.ResponseYes then process1 else process2
        Nothing -> do
            ans <- G.dialogRun dia
            if null xs then
                G.dialogResponse dia ans
            else do
                maybe (G.dialogResponse dia ans) (ifOk ans dia) mprocess1
                maybe (G.dialogResponse dia ans) (ifOk ans dia) mprocess2
    --G.widgetDestroy dia
    where ifOk ans dia prcs = if ans == G.ResponseYes then
                                G.widgetDestroy dia >> prcs
                              else G.widgetDestroy dia

--runQuestionMessage,runWarningMessageWindow,runInfoMessage,runErrorMessage
-- executing none, one process or two processes in a window
runQuestionMessage mStr string =
    runQuestionMessageGen mStr string Nothing Nothing
runDecisionMessageWindow mStr string process =
    runQuestionMessageGen mStr string (Just process) Nothing
runDoubleDecisionMessageWindow mStr string p1 p2 =
    runQuestionMessageGen mStr string (Just p1) (Just p2)

--
-- general dialog run
data DialogType = Warning | Question | Info | Error

runMsgDialog :: Maybe String -> String -> DialogType -> IO()
runMsgDialog  mStr str  Warning  = runWarningMessageWindow mStr str
runMsgDialog  mStr str  Question = runQuestionMessage mStr str
runMsgDialog  mStr str  Info     = runInfoMessage mStr str
runMsgDialog  mStr str  Error    = runErrorMessage mStr str

----}
--
actionsOnToggle :: G.ToggleButtonClass a => a -> IO() -> IO () -> IO()
actionsOnToggle toggleButton  actionTrue actionFalse =
     (G.on toggleButton G.toggled $
        G.toggleButtonGetActive toggleButton >>= \a -> if a then actionTrue else actionFalse) >>
      return ()

----
toggleMessageifChange :: G.ToggleButtonClass a => a -> String -> IO (G.ConnectId a)
toggleMessageifChange button message = do
    G.on button G.toggled $  do --G.tryEvent $
        isActive <- G.toggleButtonGetActive button
        if isActive then
            return ()
        else do
            dia <- newMessageQuestion message
            ans <- G.dialogRun dia
            if ans == G.ResponseYes then
                G.widgetDestroy dia
            else do
                G.set button [G.toggleButtonActive G.:= True]
                G.widgetDestroy dia
-----------------------------------------------------------------------------------------------------
 -- modify the alignment options if the button is selected
setAlignOptions :: G.ToggleButtonClass self => IORef a -> (t -> a -> a) -> (self, t) -> IO ()
setAlignOptions mainRef f (button, aopt) = do
    setAlignOptions_aux mainRef f (button, aopt) >>
         ( setAlignOptionsOnChange mainRef f (button, aopt)  >> return ())
    where
        setAlignOptions_aux mainRef f (button, aopt) = do
            active <- G.toggleButtonGetActive button -- (modifyAlignOpt aopt)
            if active then modifyIORef mainRef (f aopt ) else return ()
            --
        setAlignOptionsOnChange mainRef f (button, aopt) = do
            G.on button G.toggled $ setAlignOptions mainRef f (button, aopt)

--- modify the refer on a change in the combo box
setRefOnComboOption :: G.ComboBoxClass self => IORef a -> (String -> a -> a) -> self -> IO ()
setRefOnComboOption mainRef f combo = do
    setRefOnComboOption_aux mainRef f combo >>
        (setRefOnComboChange mainRef f combo >> return ())
    where
        setRefOnComboOption_aux mainRef f combo = do
            G.comboBoxGetActiveText combo >>= maybe (return ()) (\sg -> modifyIORef mainRef $ f sg)
        --
        setRefOnComboChange mainRef f combo = do
            G.on combo G.changed $ setRefOnComboOption mainRef f combo
----------------------------------------------------------------------------------------------------
myModifyIORef io f = atomicModifyIORef io (\a -> (f a , ()))
----------------------------------------------------------------------------------------------------
remSp xs = filter (not.isSpace) xs
--
entVal :: String -> Int
entVal xs | valY xs = (read xs)::Int
          | otherwise = -1
valY xs = let noSp = (remSp xs) in
            not (null noSp) &&  all isDigit noSp

----
fst3 (a,_,_) = a
snd3 (_,a,_) = a
trd (_,_,a)  = a

----
bring2Front :: Eq a => a -> [a] -> [a]
bring2Front y xs = bring2FrontBy (==) y xs

-- b2f a xs = let (ps,qs) = break (== a) xs in if null qs then ps else (head qs : ps) ++ (tail qs)
bring2FrontBy1 :: (a -> a -> Bool) -> [a] -> [a]
bring2FrontBy1 f   []   =  []
bring2FrontBy1 f (x:xs) =  bring2FrontBy f x xs
--

bring2FrontBy :: (a -> a -> Bool) -> a -> [a] -> [a]
bring2FrontBy f a xs  | null rest = a : front
                      | otherwise =  head rest : front ++ (tail rest)
    where (front, rest) = break (f a) xs

--
bring2FrontBy' f a xs = bring2FrontBy f a (changeFirst a xs)
    where
        changeFirst :: Eq b => (String,b,c) -> [(String,b,c)] -> [(String,b,c)]
        changeFirst    _  [] = []
        changeFirst  (a,b,c) (x : xs) | b == snd3 x  = (a,b,c) : changeFirst (a,b,c) xs
                                      | otherwise    =   x   : changeFirst (a,b,c) xs

--
insert x xs | x `elem` xs = bring2Front x xs
            | otherwise   = x:xs

--- maybe3: returns Just if all 3 maybes are a Just or nothing otherwise
maybe3 :: Maybe a -> Maybe b -> Maybe c -> Maybe (a,b,c)
maybe3  ma mb mc =
    case maybe2 (maybe2 ma mb) mc of
        Just ((a,b),c) -> Just (a,b,c)
        _ -> Nothing

--- maybe2: returns Just if both maybes are a Just or nothing otherwise
maybe2 :: Maybe a -> Maybe b -> Maybe (a,b)
maybe2 =  liftM2 (,)
--------------------------------------------------------------------------------------------------
--- updates a text buffer with a string at the end
updateBufferAtEnd :: G.TextBuffer -> String -> IO()
updateBufferAtEnd bf str = do
    G.textBufferGetEndIter bf >>= flip (G.textBufferInsert bf) (str ++"\n")

-- clears the text buffer
clearTextBuffer :: G.TextBuffer -> IO()
clearTextBuffer bf = do
    start <- G.textBufferGetStartIter bf
    G.textBufferDelete bf start =<< G.textBufferGetEndIter bf
--
{-- add a buffer at the
appendTextBuffer :: G.TextBuffer -> String -> IO ()
appendTextBuffer tb str = G.postGUIAsync $ do
    endItr <- G.textBufferGetEndIter tb
    G.textBufferInsert tb endItr (str ++ "\n")
--}
--------------------------------------------------------------------------------------------------
toggleEntyOnFocusOut :: G.EntryClass self => self
     -> String
     -> Maybe String
     -> String
     -> IO () -- (G.ConnectId self)

toggleEntyOnFocusOut entry message mmsg defaultString = do
    let mx = maybe "" (\st -> " "++st) mmsg
    currentEntryText <- G.entryGetText entry
    let pYes = G.entrySetText entry currentEntryText
    let pNo  = G.entrySetText entry defaultString
    let displayMesg = runDoubleDecisionMessageWindow Nothing (message++currentEntryText++mx) pYes pNo
    toggleEntyOnFocusOutGen entry (Just defaultString) displayMesg

    {-
    entry `G.on` G.focusOutEvent $  G.tryEvent $ liftIO $ do
        currentEntryText <- G.entryGetText entry
        let mx = maybe "" (\st -> " "++st) mmsg
        if currentEntryText == defaultString then
                liftIO $ return ()
        else do
            let pYes = G.entrySetText entry currentEntryText
            let pNo  = G.entrySetText entry defaultString
            runDoubleDecisionMessageWindow (message++currentEntryText++mx) pYes pNo
     --}
--- a more General version of EntryFocusout even
toggleEntyOnFocusOutGen :: G.EntryClass self => self  -> Maybe String -> IO () -> IO ()
toggleEntyOnFocusOutGen entry mDefault onFocusOut = do
    let mx = maybe "" (\st -> " "++st) mDefault
    (entry `G.on` G.focusOutEvent $ G.tryEvent $ liftIO $ do
        --currentEntryText <- G.entryGetText entry
        case mDefault of
            Just  string -> do
                currentEntryText <- G.entryGetText entry
                if currentEntryText == ""  then
                    G.entrySetText entry string >> return True
                else return False
            Nothing -> return False --  G.entrySetText entry string
        onFocusOut) >> return ()
-- toggleEntyOnFocusOutGen entry message mmsg defaultString

-- convenience function ro create a doalog box and execute a process
processDialogNew :: G.WidgetClass widget =>  Maybe widget -> [(G.Button , IO ())] -> IO G.Dialog
processDialogNew  mw buttonsAndProcesses = do
    dia <- G.dialogNew
    upper <- G.dialogGetUpper dia
    -- pack itnto teh opper ared
    maybe (return ()) (\s -> do
                            G.widgetShow s
                            G.boxPackStart upper s G.PackGrow 3) mw
    lower <- G.dialogGetActionArea dia
    mapM_ (uncurry (attachPButt lower dia)) buttonsAndProcesses
    --
    bb <- G.dialogAddButton dia "Cancel" G.ResponseClose
    G.on bb G.buttonActivated $ G.widgetDestroy dia -- process
    -- handle for the process button
    return dia
        where
           attachPButt hbox dia pButton proc = do
                G.boxPackEnd hbox pButton G.PackNatural 3
                (G.on pButton G.buttonActivated $  proc) >> G.widgetDestroy dia



----- convenience functions for creating series of buttons, etc
checkBoxWithLeftLabel :: String -> IO (G.CheckButton,G.HBox)
checkBoxWithLeftLabel string  = do
    chkButton <- G.checkButtonNew -- labelName labelWidth widget packWidgetNatural
    hBox <- labelLeftofWidgetE' string (6 * length string ) chkButton True -- idget string chkButton True
    return (chkButton, hBox)

--- combo box with left label
comboBoxWithLeftLabel :: String -> IO (G.ComboBox,G.HBox)
comboBoxWithLeftLabel string  = do
    combo <- G.comboBoxNewText
    hBox <- labelLeftofWidget string combo False
    return (combo, hBox)

--- takes a list of strins and creates a list of radio
--- buttons, in the same group, with the strings as names
makeRadioButtonGroupFromStrings :: [String] -> IO [G.RadioButton]
makeRadioButtonGroupFromStrings     []     = return []
makeRadioButtonGroupFromStrings (str:strs) = do
    butt <- G.radioButtonNewWithLabel str
    makeRadioButtonGroupFromStrings_aux butt strs
    where
        makeRadioButtonGroupFromStrings_aux butt []         = return [butt]
        makeRadioButtonGroupFromStrings_aux butt (str:strs) = do
            butt1 <- G.radioButtonNewWithLabelFromWidget butt str
            butts <- makeRadioButtonGroupFromStrings_aux butt1 strs
            return (butt : butts)

---- returns a HBox with a label, with labelName,  to the left of widget
labelLeftofWidget labelName widget packWidgetNatural = do
    label <- G.labelNew (Just labelName)
    hBox <- G.hBoxNew False 1
    G.boxPackStart hBox  label G.PackNatural 3
    if packWidgetNatural then
        G.boxPackStart hBox  widget G.PackNatural 3
    else
        G.boxPackStart hBox  widget G.PackGrow 3
    return hBox
--
labelLeftofWidgetLabel tf label widget packWidgetNatural = do
    hBox <- G.hBoxNew False 0
    if packWidgetNatural then
        G.boxPackEnd hBox  widget G.PackNatural 2
    else
        G.boxPackEnd hBox  widget G.PackGrow 2
    if tf then
        G.boxPackStart hBox  label G.PackNatural 1
    else
        G.boxPackEnd hBox  label G.PackNatural 1
    return hBox
--
---- returns a HBox with a label, with labelName,  to the left of widget -- packing at ther end
labelLeftofWidgetE labelName labelWidth widget packWidgetNatural =
    labelLeftofWidgetEGen True labelName labelWidth widget packWidgetNatural

labelLeftofWidgetE' labelName labelWidth widget packWidgetNatural =
    labelLeftofWidgetEGen False labelName labelWidth widget packWidgetNatural

labelLeftofWidgetEGen stEn labelName labelWidth widget packWidgetNatural = do
    label <- G.labelNew (Just labelName)
    G.widgetSetSizeRequest label labelWidth  (-1)
    -- G.set label [G.miscXalign G.:= 1 / (fromIntegral justification) ]
    --G.set label [G.labelJustify G.:= justification ]
    hBox <- G.hBoxNew False 0
    if packWidgetNatural then
        G.boxPackEnd hBox  widget G.PackNatural 3
    else
        G.boxPackEnd hBox  widget G.PackGrow 3
    --
    if stEn then
        G.boxPackStart hBox  label G.PackNatural 2
    else
        G.boxPackEnd hBox  label G.PackNatural 2
    --G.boxPackEnd hBox  label G.PackNatural 0
    return hBox
---------------------------------------------------------------------------------------------
-- creeate a scrolled entry area
data TScroll = SNone | HScroll | VScroll | BScroll
--
setScrollPolicy :: TScroll -> (G.PolicyType, G.PolicyType)
setScrollPolicy SNone   = (G.PolicyNever , G.PolicyNever)
setScrollPolicy HScroll = (G.PolicyAutomatic, G.PolicyNever)
setScrollPolicy VScroll = (G.PolicyNever, G.PolicyAutomatic)
setScrollPolicy BScroll = (G.PolicyAutomatic, G.PolicyAutomatic)

setVScrollPolicy = snd . setScrollPolicy
setHScrollPolicy = fst . setScrollPolicy

makeScrolledEntryArea ::  TScroll -> Bool -> Bool -> IO (G.ScrolledWindow , G.TextBuffer )
makeScrolledEntryArea  sp entryAreaFocus entryAreaSensitive = do
    -- the display window
    inputBuffer <- G.textBufferNew Nothing
    inputSpace <-  G.textViewNewWithBuffer inputBuffer
    G.set inputSpace [G.textViewWrapMode G.:= G.WrapWord, G.widgetCanFocus G.:= entryAreaFocus,
                      G.widgetSensitive G.:= entryAreaSensitive ]
    scrwin <- G.scrolledWindowNew Nothing Nothing
    G.set scrwin [G.scrolledWindowHscrollbarPolicy  G.:= setHScrollPolicy sp,
                  G.scrolledWindowVscrollbarPolicy  G.:= setVScrollPolicy sp]
    --G.scrolledWindowAddWithViewport scrwin inputSpace
    G.containerAdd scrwin inputSpace
    return (scrwin,inputBuffer)
------------------------------------------------------------------------------------

--- creates a vertical scrolled window from a list of widgets
vScrolledWinWithWgtList ::  G.WidgetClass widget => [widget] -> TScroll -> IO G.ScrolledWindow
vScrolledWinWithWgtList widgets  sp = do
    --table <- G.tableNew (length widgets) 2 False
    table <- tableWithWidgetList True widgets False 1 --  -> [widget] -> Bool -> Int -> IO G.Table
    chainsScrollWin <- G.scrolledWindowNew Nothing Nothing
    G.scrolledWindowSetPlacement chainsScrollWin G.CornerTopRight
    G.set chainsScrollWin [G.scrolledWindowHscrollbarPolicy  G.:= setHScrollPolicy sp,
                           G.scrolledWindowVscrollbarPolicy  G.:= setVScrollPolicy sp]
    G.scrolledWindowAddWithViewport chainsScrollWin table
    --mapM_ (\(bu, x) -> G.tableAttachDefaults table bu 0 1 x (x+1)) $ zip widgets [0..]
    return chainsScrollWin

---
--- creates a single-column table from a list of widgets
-- vertical, horizontal
tableWithWidgetList :: G.WidgetClass widget => Bool -> [widget] -> Bool -> Int -> IO G.Table
tableWithWidgetList isVertical  widgets homo spacing
    | isVertical = do
        table <- G.tableNew (length widgets) 1 homo
        mapM_ (\(bu, x) -> do
                    G.tableAttachDefaults table bu 0 1 x (x+1)
                    G.set table [G.tableChildXPadding bu G.:= spacing]
                    -- mapM_ (setPadding 4) [a1,a,b,c,d]
    --
                    ) $ zip widgets [0..]
        G.tableSetRowSpacings table spacing
        return table
    | otherwise = do
        table <- G.tableNew  1 (length widgets) homo
        mapM_ (\(bu, x) -> do
                    G.tableAttachDefaults table bu  x (x+1) 0 1
                    G.set table [G.tableChildXPadding bu G.:= spacing]
                    ) $ zip widgets [0..]
        G.tableSetColSpacings table spacing
        return table


{--- creates a single-row table from a list of widgets
tableWithVwidgetList :: G.WidgetClass widget => [String] -> [widget] -> IO G.Table
tableWithVwidgetList names widgets  = do
    labels <- mapM (G.labelNew . Just) names
    table <- G.tableNew (length names) 4 False
    let lws = zip (labels, widgets)
    let h n = n `mod` 4
    mapM_ (\((l,w), x) -> do
                G.tableAttachDefaults table l (h x) (1 + h x) x (x+1)) $ zip lws [0..]
    return table
--}
---------------------------------------------------------------------------------------------
--  NOTEBOOKS
---------------------------------------------------------------------------------------------
notebookWithScrollWin :: G.WidgetClass w => Bool -> [(w,String)] -> Bool -> IO (Maybe G.Frame, G.Notebook)
notebookWithScrollWin hasScroll contents inFrame = do
    -- has scroll is true if the widget has native scrolling, e.g, a TextView
    let (xs,ys) = unzip contents
    ks <- mapM  (putInScrolledWin hasScroll) xs
    notebookInFrame (zip ks ys) inFrame
    -- (Just G.stockExecute)
    where
        putInScrolledWin bb w = do
            scrWin <- G.scrolledWindowNew Nothing Nothing
            G.scrolledWindowSetPlacement scrWin G.CornerTopLeft
            G.set scrWin [G.scrolledWindowHscrollbarPolicy  G.:= G.PolicyAutomatic,
                          G.scrolledWindowVscrollbarPolicy  G.:= G.PolicyAutomatic]
            if bb then
                G.containerAdd scrWin w
            else
                G.scrolledWindowAddWithViewport scrWin w
            return scrWin

-- convenience function for adding a notebook to a frame
notebookInFrame  :: G.WidgetClass w =>  [(w,String)] -> Bool -> IO (Maybe G.Frame, G.Notebook)
notebookInFrame contents inFrame = do
    --contents' <- mapM (\(w,s) ->  G.labelNew (Just s) >>= \l -> return (w,l)) contents
    frame <- G.frameNew
    G.notebookNew >>= \ntbk -> do
    G.set ntbk [G.notebookScrollable G.:= True, -- G.notebookHomogeneous G.:= True,
                G.notebookEnablePopup G.:= True, G.notebookTabBorder G.:= 4,
                G.notebookEnablePopup G.:= True, G.notebookShowBorder G.:= True]
    G.on  ntbk G.pageAdded $ (\ _ i -> G.set ntbk [G.notebookCurrentPage G.:= 0])
    ---- add pages to the notebook
    --mapM_ (\nb -> G.widgetModifyBg nb G.StateNormal (G.Color 105 105 105)) (map fst contents)
    mapM_ (G.widgetShow . fst) contents
    --labels <- mapM (G.labelNew . Just)  contents
    --let notebookAppendPageMenu tab l ch G.notebookAppendPageMenu
    --mapM labelFromString contents >>= mapM_ (\(a,b,c) -> G.notebookAppendPageMenu ntbk a b c)
    mapM_ (uncurry $ G.notebookAppendPage ntbk) contents
    ----
    mframe <- if inFrame then  do
                   G.containerAdd frame ntbk
                   return (Just frame)
              else
                   return Nothing
    G.widgetShow ntbk
    return (mframe, ntbk)

--- adds a widget to a NoteBook, given a string for the label.
-- includes a button for closing the page
noteBookAddWidget :: (G.NotebookClass self, G.WidgetClass child) => self ->
    String ->                   -- ^ The string for the tab title
    IORef [(String, Int)] ->    -- ^ This should have the tab title and its index. Initialize an
                                -- ^ IORef with an empty list and continue to pass it into the list
    Maybe (IORef [a]) ->        -- ^ Possible action  to do when adding an extra page
    (Int -> IO ()) ->           -- ^ a some action to do on a switch page
    child ->                    -- ^ The widget to add to the notebook
    IO ()
noteBookAddWidget noteb tabTitle defectRef toDoOnAdd fF widget = do
    G.widgetShowAll widget
    -- set up the label
    menuLabel <- G.labelNew (Just tabTitle)
    button <- G.buttonNewWithLabel "x"-- G.buttonNewFromStock G.stockClose
    let width = 6 * length tabTitle
    !menuBox <-  labelLeftofWidgetE' (tabTitle++" ") width button True
    G.widgetShowAll menuBox
    --
    list <- readIORef defectRef
    case find ((== tabTitle) . fst) list of
        Just (_,i) ->
            G.notebookSetCurrentPage noteb i
        Nothing -> do
            !index <- G.notebookAppendPageMenu noteb widget menuBox menuLabel
            modifyIORef defectRef (++[(tabTitle,index)] )
    G.on button G.buttonActivated $ do
        list <- readIORef defectRef
        case find ((== tabTitle) . fst) list of
            Just (_,i) -> do
                G.notebookRemovePage noteb i
                modifyIORef defectRef (adjust . deleteBy (==) (tabTitle,i))
                maybe (return ()) (flip modifyIORef (removeAt (i+1))) toDoOnAdd
            Nothing -> return ()
    --return () -- need this, otherwise will return IO (G.ConnectId G.Button), re: button handler
    -- handle for switchin page
    G.on noteb G.switchPage $ fF
    return ()
    where
        --- | readjust the tab indices after one was deleted
        adjust :: [(a,Int)] -> [(a,Int)]
        adjust       []                   = []
        adjust  ys@((a,x):xs) | x /=0     = adjust_aux ((a,x-1) : xs)
                              | otherwise = adjust_aux ys
            where
                adjust_aux ((a,x):(b,y):ys)
                    | x /= (y - 1) = (a,x) : adjust_aux ((b,(y-1)) : ys)
                    | otherwise    = (a,x) : adjust_aux ((b,y) : ys)
                adjust_aux  xs     = xs


--------------------------------------------------------------------------------------------
--  SCROLLED AREAS
--------------------------------------------------------------------------------------------
-- vScrolledWinWithWgtList, hPanedArea, vPanedArea
-- creates a horizontal and vertical panes pane with widgets for the left and right,
-- or top and botton, respectively.
--hPanedArea :: G.Frame -> G.Frame -> Int -> IO (G.HPaned)
hPanedArea w1  w2  n = do
    lr <- doublePanedArea w1 w2 True n
    case lr of
        Left hpane -> return hpane
        _          -> fail "no right pane"

-- vertical panes
--vPanedArea :: G.Frame -> G.Frame -> Int ->  IO (G.VPaned)
vPanedArea w1  w2 n = do
    lr <- doublePanedArea w1 w2 False n
    case lr of
        Right vpane -> return vpane
        _           -> fail "no left pane"

-- general function
doublePanedArea :: (G.WidgetClass w, G.WidgetClass w1) => w -> w1 -> Bool -> Int -> IO (Either G.HPaned G.VPaned)
doublePanedArea  widgetL widgetR lor n
    | lor       = G.hPanedNew >>= liftM Left . packPane widgetL widgetR  n
    | otherwise = G.vPanedNew >>= liftM Right . packPane widgetL widgetR n
    where
        packPane widgetL widgetR n pane  = do
            [lframe, rframe] <- sequence $ replicate 2  G.frameNew
            G.set lframe [G.containerChild G.:= widgetL]
            G.set rframe [G.containerChild G.:= widgetR]
            G.panedPack1 pane lframe True True
            G.panedPack2 pane rframe False True
            G.panedSetPosition pane n -- 550
            return pane
-----------------------------------------------------------------
--- widget left of widget -with control of how the leftmose widget is packed
--- The right widget is always packed to greo
widgetLeftWifget :: (G.WidgetClass w, G.WidgetClass w1) => Bool -> w -> w1 ->  IO G.HBox
widgetLeftWifget ng wl wr  = do
    mHbox <- G.hBoxNew False 1
    let natGrow = if ng then G.PackNatural else G.PackGrow
    G.boxPackStart mHbox wl natGrow  1
    G.boxPackEnd mHbox wr G.PackGrow  1
    return mHbox

-- horizontals list of similart widgets packed in a box, with
-- or without a frame around them
horizontalWidgetList :: (G.WidgetClass w)=>  Bool -> [w] -> Bool -> IO (G.HBox,[w])
horizontalWidgetList  natural widgets framed  = do
    mHbox <- G.hBoxNew False 1
    let packing = if natural then  G.PackNatural else  G.PackGrow
    mapM_ (\w -> G.boxPackStart mHbox w packing 3) widgets
    if framed then do
        frm <- G.frameNew
        mBox1 <- G.hBoxNew False 1
        G.containerAdd frm mHbox
        G.boxPackStart mBox1 frm packing  1
        return (mBox1, widgets)
    else return (mHbox, widgets)
------------------------------------------------------------------------------------


