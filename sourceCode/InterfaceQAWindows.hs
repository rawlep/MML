----------------------------------------------------------------------------------------------------
{- |
Module : InterfaceQAWindows
Copyright : (c) Rawle Prince 2012
License : GPL-style

Maintainer : Rawle Prince (rawle DOT prince at GMAIL dot com )
Stability : provisional
Portability : uses gtk2hs

Interface for analysis funcitons in PPDL

-}
----------------------------------------------------------------------------------------------------
-- TO DO:
-- 1. scheme box
-- 2. network analysis box
-- 3. redo the analysis box to omit the analysis of individual secttions
--    (the individual section option shold not be on the top-level options window)
----------------------------------------------------------------------------------------------------



module InterfaceQAWindows (analysisBoxForAFrame--globalAnalysisTop
    ,fillPreviewBox
    -- ,mkResultBox --
    ,schemAnl)
    where -- topOfWindow,schemePrepAndAnanlysis,


----------- native haskell modules ------------------------
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Graphics.UI.Gtk as G
import Numeric.LinearAlgebra (fromList)
import Data.Ord (comparing)
import Data.IORef
import Data.List (find, delete,deleteBy,intersectBy, intersect, findIndex, nub,foldl', nubBy,maximumBy,sortBy)
import System.Directory (doesDirectoryExist,createDirectory) -- doesFileExist,removeFile)
import Data.Char (isDigit,isAlpha,isAlphaNum)
import Data.Maybe (isJust,fromMaybe,maybe,listToMaybe)
--import Control.Monad.Trans (liftIO)
import Control.Parallel (pseq)
import Control.Monad (liftM,liftM2)
import Control.Concurrent (forkIO,killThread)
import System.Random (newStdGen) -- , randomRs)
import System.Directory (doesFileExist)
--import Control.Monad (when)

---- specific modules
import ProcessFile (Display, display,readRoadTypeSL,intercalateFormList,getRoadClasses,
                    getRoadNamesAndSections,getRoadNames)
import HMDIFPrelude (Defect (..) , typesAndDetails,scode2align,(<>))
import GlobalParameters -- (AddedCombos,labelLeftofWidget,labelLeftofWidgetE)
import RprGD2 (mml2DsGD, OptimisationType(..))
import LMisc (myTake,force,mkIntervals)
import ReadFromDataFiles2
import RprMix2 (OModel,printElm,Mmodel (MkModel),r_n)
import Parallel
import FileIO
import ResultTypes (TdsParms (..))
import DrawingFrame (getDrawingCanvas,fileBrowser,isValid,mkDnaDisplayFrame
                     ,comboText,updateDrawingArea,mkProgressionGroupList,mkDnaTable) -- vals2drwRef,mkDnaBase,
--
----------------------------------------------------------------------------------------------------

setName string  | firstletter == 'R' = Rutting
                | firstletter == 'C' = Cracking
                | firstletter == 'T' = Texture
                | otherwise          = Profile
    where firstletter =  head string


-- emptying a text buffet
myEmptyTextBuffer buffer = do
    (start,end) <-  G.textBufferGetBounds buffer
    G.textBufferDelete buffer start end

----------------------------------------------------------------------------------------------------
-- Interface for global analysis options
--
data AWin = AWin {
        -- the file path to open
        fPath :: String,
        -- the list of selected condition condes
        sList :: [Defect],
        -- a list of options corresponding to the selected condition codes
        -- i.e. (years, distance, negslopes) for each condition
        options :: [(Int,Int,Bool)],
        fType   :: FileType
     }

--- parameter to pass to the preview box
data PreviewParms = PreviewParms {
    wrtFun :: (String -> IO()),
    yrsEnt :: G.Entry,
    distEnt :: G.Entry,
    nSpopes :: G.CheckButton,
    schemAnl :: G.RadioButton
    }
--}

---------------------------------------------
--- flags for the extract or analyze buttons
--- extract, analyse, extNotAnl
extract :: Maybe Bool
extract = Just False

analyse :: Maybe Bool
analyse = Just True

extNorAnl :: Maybe Bool
extNorAnl = Nothing
---------------------------------------------


--- the geberal interface for analysis
--[G.RadioButton]
analysisBoxForAFrame ::  Bool -> [G.RadioButton] ->
     G.Entry  -> [G.VBox] ->   IO G.VBox
analysisBoxForAFrame tf rbutts fe boxes = do
    aBox <- G.vBoxNew False 5
    selectBox <- getSelectBox tf rbutts fe -- this comes from another function
    detailsFrame <- G.frameNew
    G.set detailsFrame [G.frameLabel G.:= " Condition parameters" ]
    table <- tableWithWidgetList False boxes False 2
    --table <- hPanedArea (boxes !! 0) (boxes !! 1) 100
    G.containerAdd detailsFrame selectBox
    G.boxPackStart aBox detailsFrame G.PackNatural 5
    G.boxPackStart aBox table G.PackGrow 2 -- detailsFrame
    return aBox
    --
    where
        getSelectBox :: Bool -> [G.RadioButton] -> G.Entry  -> IO G.HBox
        getSelectBox sE rbutts fileEntry = do
            [thbox,entryBox, tablesBox] <- sequence $ replicate 3 (G.hBoxNew False 2)
            browseButton <- G.buttonNewWithLabel " Load data file "
            --- pack the entry box
            G.boxPackEnd entryBox fileEntry G.PackGrow 4
            G.boxPackStart entryBox browseButton G.PackNatural 2
            G.on browseButton G.buttonActivated $ do
                fileBrowser (Just fileEntry) >>= G.dialogRun  >>= \_ -> return () -- ) >> return ()
            --- put the condition chooser in a box
            let n = length rbutts
            let (condTypes, scnSem) = splitAt (n-2) rbutts
            condBox <- tableWithWidgetList False condTypes False 3
            vButtonTable <- tableWithWidgetList True scnSem True 3
            G.vSeparatorNew >>= \sep -> do
                sep1 <- G.vSeparatorNew
                G.boxPackStart tablesBox  sep1 G.PackNatural 0
                G.boxPackStart tablesBox  vButtonTable G.PackNatural 8
                G.boxPackStart tablesBox  sep G.PackNatural 0
                G.boxPackStart tablesBox  condBox G.PackGrow 5
                --
                --G.containerAdd tablesFrame tablesBox
                if sE then do
                    G.boxPackStart thbox  entryBox G.PackGrow 2
                    G.boxPackStart thbox  tablesBox G.PackGrow 0
                else do
                    G.boxPackStart thbox  tablesBox G.PackGrow 2
                    G.boxPackStart thbox  entryBox G.PackGrow 0
            --
            --detailsFrame <- G.frameNew
            return thbox
   --}
{-
--------------------------------------------------------------------------------------------------
--    Dialog to initialize the network level analyis and related dialiogos and datatypes
--------------------------------------------------------------------------------------------------
--- the maybe here should be a pair of tracking panes
globalAnalysisTop :: Maybe G.Window ->
    Maybe (IORef [Road]) ->
    IO (G.VBox, Maybe AWin,[G.RadioButton], G.Entry , PreviewParms)
globalAnalysisTop  mwin strAndChn = do
    [mainVBox,listsVBox] <- sequence $ replicate 2 (G.vBoxNew False 1)
    [bottomHBox,topHBox, -- condsVBox, topHBox,bottomOBox,
        buttsVBox] <- sequence $ replicate 3 (G.hBoxNew False 1)
    comboList <- sequence $ replicate 5 G.comboBoxNewText
    vsep   <- G.vSeparatorNew
    [hsep, hsep1, hsep2]   <- sequence $ replicate 3 G.hSeparatorNew
    [yearsEntry, distEntry, fileEntry] <- sequence $ replicate 3 G.entryNew
    -- fileEntry is the entry for the file Path. set the size of the entry
    G.widgetSetSizeRequest fileEntry 360 (-1)
    -------------------------------------------------------------------
    prevAnalFrame <- G.frameNew
    -- condTypes
    ------------------------------------------------------------------------------------
    -- the display window
    -- the progress and display buffers
    (scrwin,progressBuffer) <- makeScrolledEntryArea BScroll False True
    G.widgetSetSizeRequest scrwin 60 (-1)
    -------- and updating chains will be added here
    (selectedCondsTable, dispBuffer) <- getSelectedConditionsTable
    ---
    let logProgress   = updateBufferAtEnd progressBuffer
    let add2SeletList = updateBufferAtEnd dispBuffer
    ----- check for extracted conditons
    checkIfExtracted False True logProgress
    -------------------------------------------------------------------
    G.set prevAnalFrame [G.frameLabel G.:= " Condition attribures" ]
    G.containerAdd prevAnalFrame listsVBox -- listsVBox
    G.boxPackStart topHBox prevAnalFrame G.PackGrow 6
    -------------------------------------------------------------------
    mapM_ (\ent -> do
                G.entrySetWidthChars ent 6
                G.entrySetMaxLength ent 6) [yearsEntry, distEntry]
    let [detailsCombo,rNameCombo,rClassCombo,rCodeCombo,srCodeCombo] = comboList
    -- fillToMaxLegth :: [String] -> [String]mapM (G.labelNew . Just)
    let comboNames = ["Road Class: ", "Road Name: ", "SCANNER Section Code: "," SCRIM Section Code: "]
    --mapM_ (\w -> G.miscSetAlignment w 1 0.5) comboNames
    let schms = zip (fillToMaxLegth comboNames)  [rClassCombo,rNameCombo,rCodeCombo,srCodeCombo]
    --let labelAndWidget m a b = labelLeftofWidget a b m
    comboBoxesWithLables <- mapM (\(l,w) -> labelLeftofWidget l w False ) (take 2 schms)
    comboBoxesWithLables1 <- mapM (\(l,w) -> labelLeftofWidget l w False ) (drop 2 schms)
    -- create table
    row1 <- tableWithWidgetList False comboBoxesWithLables False 2
    row2 <- tableWithWidgetList False comboBoxesWithLables1 False 2
    --schmAndClass <- tableWithWidgetList True [row1,row2] True 2
    ------------------------------------ }
    comboRef <- newIORef [] --- the a record to toggle the condition details
    --roadConditionRef
    --- selected,
    detailsListRef <- newIORef ([],[],[])
    advancedRef <- newIORef ([],Nothing)
    dataTypeRef <- newIORef SCANNER -- toggles the curreltly selected data type
    countRef <- newIORef 1 -- a counter ref track what is happings
    rNameRef <- newIORef []
    extractAnal <- newIORef Nothing --- flag to toggle extraction or analysys
    ------------------------------------
    let comboLabels = [" Condition Detail: "]
    let comboLabelPairs = zip comboLabels comboList
    vBoxesWithLabels <- mapM (\(st,w) -> labelLeftofWidget st w False) comboLabelPairs
    ---
    [rcl,rsc] <-  mapM G.radioButtonNewWithLabel ["Road Class","Section Code"]
    G.radioButtonSetGroup rcl rsc
    --mapM_ (\b -> G.boxPackEnd (vBoxesWithLabels !! 0) b G.PackNatural 6) [rcl,rsc]
    -------------------------------------------------------------------------------
    ---- fork the population of the roads, etc
    ---  We will need to put each road class in a separate file. This wouuld be done when
    ---  When the details of the roas is added
    -- desensetise the combos :
    mapM_ (flip G.widgetSetSensitive False) (map snd schms)
    rtypes <- getRoadClasses >>= return . filter (isAlpha . snd)
    let roadTypes =  map snd $ nubBy (\a b -> snd a == snd b) rtypes
    ----- extract the road classes
    let getClasses = mapM_ (G.comboBoxAppendText rClassCombo) $ map ( :"-roads") roadTypes
    --let getNames   = mapM_ (G.comboBoxAppendText rNameRef) $ map L.unpack rNames
    --forkIO $ mapM_ (G.comboBoxAppendText rCodeCombo) $ map (\(a,_,_) -> L.unpack a) secAndTypes
    -- clearAdd cRef combo list
    -- let schms = zip comboNames  [rNameCombo,rClassCombo,rCodeCombo,srCodeCombo]
    rCodesComboRef <- newIORef [] --- the a record to toggle the condition details
    roadClassAdded <- newIORef False
    {--- fix this
    let getCodesFromClass = do
        index <- G.comboBoxGetActive rClassCombo
        if index >= 0 then  do
            let !codes = [bs | (bs,c) <- rtypes, c == (roadTypes !! index)]
            clearAdd rCodesComboRef rCodeCombo  $ map L.unpack codes --
        else  return ()
    --}
    G.on rClassCombo G.changed $ do
          rclsAdded <- readIORef roadClassAdded
          index <- G.comboBoxGetActive rClassCombo
          if rclsAdded then do
              let des = "Extracting classes for "++[roadTypes !! index]++"-roads"
              logProgress (des ++ " ... ")
              -- forkProcess (Just des) logProgress getCodesFromClass Nothing
          else  do
              let rtyp = roadTypes !! index
              let mkRN1 (a,b,c) = L.unpack c
              let mkSC1 (a,b,c) = L.unpack b
              logProgress ([rtyp] ++"-roads selected")
              rname <- getRoadNamesAndSections (map fst $ filter ((== rtyp) . snd) rtypes) -- >>= return $ map mkSt1
              clearAdd rNameRef rNameCombo (map mkRN1 rname)
              clearAdd rCodesComboRef rCodeCombo (map mkSC1 rname)
              G.widgetSetSensitive rNameCombo True
    --sensetize the combos once the road classes have been extracted
    forkProcess Nothing logProgress getClasses
                        (Just $ mapM_ (flip G.widgetSetSensitive True) [rClassCombo,rCodeCombo])

    --forkProcess Nothing (updateBufferAtEnd progressBuffer) getNames
    --                    (Just $ mapM_ (flip G.widgetSetSensitive True) [rNameCombo,rCodeCombo])
    -- let schms = zip comboNames  [rNameCombo,rClassCombo,rCodeCombo,srCodeCombo]
    -- clearAdd comboRef detailsCombo (map display lst)
    --- update the road class names,codes and section codes upon a election of a road class
    -------------------------------------------------------------------------
    [addButton,analButton,previewButt,clrButt] <- mapM G.buttonNewWithLabel
                                                                [" Add condition details ",
                                                                 " Analyse ", " Extract ",
                                                                 " Edit List "]
    --advOptions <- advancedOptionsWin -- (Just advancedRef)
    -- file entry box vBoxesWithLabels <- mapM (\(st,w) -> labelLeftofWidget st w False) comboLabelPairs
    let entryLabels = ["  Average maintenance frequency in years: ",
                       "  Average treatment length in kilometers: "]
    let entryPairs = zip entryLabels [yearsEntry, distEntry]
    entryTable <- mapM (\(l,w) -> labelLeftofWidget l w True) entryPairs >>= \eb ->
                        tableWithWidgetList False eb False 10
                   --     [yearsEntry, distEntry]
    -- use the dummy do that none will be set on start up
    dummy  <-  G.radioButtonNew -- WithLabel "
    [nl, sl]  <- mapM G.radioButtonNewWithLabel [" Network Level Analysis "," Scheme/road Level Analysis "]
    G.radioButtonSetGroup dummy nl
    G.radioButtonSetGroup nl sl
    ---
    ---
    --- negative slopes
    negativeSlopes  <- G.checkButtonNew -- WithLabel "
    optsBox <- labelLeftofWidgetE' "Include negative slopes " 1 negativeSlopes True
    --
    mapM_ (\w -> G.boxPackStart optsBox w G.PackGrow 2) [nl,sl]
    --G.boxPackEnd optsBox negativeSlopes' G.PackGrow 2
    -------------------------------------------------------------------------------------
    G.on addButton G.buttonActivated $ do
        -- need here: read inout parameter file
        [str1,str2] <- mapM G.entryGetText [yearsEntry, distEntry]
        negSlope <- G.toggleButtonGetActive negativeSlopes
        -- return () -- temporary
        --
        case (validateEntries [str1,str2] ["years","distance"]) of
            Left [e1,e2] -> do
                index <- G.comboBoxGetActive detailsCombo -- >>= \index ->
                if index >= 0 then do
                    (selected,_,dlist) <- readIORef detailsListRef -- >>=  \ ->  do
                    let sel = (dlist !! index)
                    let str = (display sel) -- ++ (" yrs: "++str1 ++ " dist: "++str2)
                    let message = "You have already added \'" ++ display sel  ++ "\' to\
                                  \ the list of selected conditions."
                    --
                    if sel `elem` selected then
                        runMsgDialog Nothing message Info
                    else do
                        let pm = (fromIntegral e1, fromIntegral e2,negSlope)
                        add2SeletList str
                        modifyIORef detailsListRef (\(a,ps,b) -> (sel : a,pm:ps,b))
                        -- clear the entries and neg slope
                        mapM_ (\ent -> G.entrySetText ent "") [yearsEntry, distEntry]
                        G.toggleButtonSetActive negativeSlopes False
                else
                    runMsgDialog Nothing "Negative index at condition details combo box" Error
            Right string -> runMsgDialog Nothing string Error
    ------------------------------------------------------------------------------------ }
    ---- the scanner or scrim radio butttons
    [d1,d2] <- mapM (\st -> G.radioButtonNewWithLabel st) ["", ""]
    condTypes <- mapM (G.radioButtonNewWithLabelFromWidget d1 )
                            (map fst $ init typesAndDetails)
    scnSem <-  mapM (G.radioButtonNewWithLabelFromWidget d2) [" SCANNER Data "," SCRIM Data "]
    let ontoggle butt = G.on butt G.toggled $ do
                            G.toggleButtonGetActive butt >>= \b ->
                             --- need special case for RCI
                             if b then
                                  G.buttonGetLabel butt >>=
                                    (\str ->
                                          case find ((== str) . fst) typesAndDetails  of
                                            Nothing -> return ()
                                            Just (_,lst) -> do
                                                clearAdd comboRef detailsCombo (map display lst)
                                                modifyIORef detailsListRef (\(a,b,_) -> (a,b,lst))
                                                let profileFile = "./mmlTdsFiles/Profile/currProfile.txt"
                                                L.writeFile profileFile (L.pack str)
                                    )
                             else
                                return ()
    mapM ontoggle condTypes
    ------------------------------------------------------------------------------------
    --- need to desensitive some stuff when SCRIM is selected
    let desensetiveForScrim butt = G.on butt G.toggled $ do
                                     G.toggleButtonGetActive butt >>= \b ->
                                        if b then do
                                            mapM_ (flip G.widgetSetSensitive False) condTypes
                                            modifyIORef dataTypeRef (\_ -> SCRIM)
                                        else do
                                            mapM_ (flip G.widgetSetSensitive True) condTypes
                                            modifyIORef dataTypeRef (\_ -> SCANNER)
    desensetiveForScrim (scnSem !! 1)

    ------------------------------------------------------------------------------------
    -- The operation we want here is to opena a dialog with options to remove
    -- entries from the list. Either indivdually selected items or all items can
    -- be removed from the list
    G.on clrButt G.buttonActivated $
        openEditDialog comboRef detailsCombo detailsListRef dispBuffer
    ---------------- PACKING ------------------------------------------------
    G.boxPackStart listsVBox optsBox G.PackNatural 5
    G.boxPackStart listsVBox (vBoxesWithLabels !! 0) G.PackNatural 5
    G.boxPackStart listsVBox entryTable G.PackNatural 5
    G.boxPackStart listsVBox hsep1 G.PackNatural 5
    --G.boxPackStart listsVBox schmAndClass G.PackNatural 2
    ---
    G.boxPackStart listsVBox hsep G.PackNatural 5
    mapM_ (\b -> G.boxPackStart buttsVBox b G.PackNatural 2) [analButton,previewButt ]
    mapM_ (\b -> G.boxPackEnd buttsVBox b G.PackNatural 4) [addButton,clrButt]
    -- put the options here
    G.boxPackStart listsVBox buttsVBox G.PackGrow 0
    --
    G.boxPackStart mainVBox topHBox G.PackNatural 6
    ------------- the bottom of the left pane -----------------------------------------.
    --- the left side has a frame highlighting the selected condition
    ---- the right the progress log
    -------- first get the table for the selected conditions. The handles for adding
    ---- this comes from the main display?
    [selectedConditions, progressLog] <- sequence $ replicate 2 G.frameNew
    G.set progressLog [G.frameLabel G.:= " Progress log" ]
    G.set selectedConditions [G.frameLabel G.:= " Selected conditions" ]
    G.containerAdd progressLog scrwin
    G.containerAdd selectedConditions selectedCondsTable
    -- tableWithWidgetList isVertical  widgets homo spacing
    displayLogs <- tableWithWidgetList False  [selectedConditions, progressLog] True 5
    G.boxPackStart  bottomHBox displayLogs G.PackGrow 6
    G.boxPackStart mainVBox bottomHBox  G.PackGrow 6
    -------------------------
    --G.widgetModifyBg mainVBox G.StateActive (G.Color 190 190 190)
    let rbutts = condTypes ++ scnSem
    -- check what has been extracted already
    --checkIfExtracted logProgress
    G.on previewButt G.buttonActivated $ do
        -- extract and align the file, bot do not analyse
        handleForAnalAndExtract extractAnal fileEntry detailsListRef dataTypeRef logProgress (analButton,False)
    --
    G.on analButton G.buttonActivated $ do
        --let btyp = (analButton,True)
        handleForAnalAndExtract extractAnal fileEntry detailsListRef dataTypeRef logProgress (analButton,True)
    ------------------------------------------------------------------------------------
    let pParms = PreviewParms logProgress yearsEntry distEntry negativeSlopes sl
    readIORef detailsListRef >>= \(sels,pms,_) -> do
        return (mainVBox, Nothing, rbutts, fileEntry,pParms)
    where
        getSelectedConditionsTable = do
            labels <- mapM (G.labelNew . Just) [" No."," Description "]
            headBox <- G.hBoxNew False 1
            displayBox <- G.vBoxNew False 1
            ---
            G.boxPackStart headBox (labels !! 0)  G.PackNatural 1
            G.boxPackEnd headBox (labels !! 1)  G.PackGrow 1
            G.vSeparatorNew >>= \sep -> G.boxPackEnd headBox sep  G.PackNatural 0
            ---
            (display, dispBuffer) <- makeScrolledEntryArea  HScroll False False
            G.boxPackStart displayBox headBox G.PackNatural 1
            G.boxPackStart displayBox display G.PackGrow 1
            return (displayBox , dispBuffer)
            --G.tableAttachDefaults entryTable (entryPairs !! 0) 0 1 0 1
        --
        validateFilePath str =
            let string = "The name entered is not a valid file path." in
                if isValid str then Nothing else (Just string)
        -- countRef
        write2Buffer tbuffer wrt string  = do
            let wrds = words string
            if not (null wrds) && head wrds == "Finished" then
                G.textBufferGetEndIter tbuffer >>= \iter ->
                    G.textBufferInsert tbuffer iter (string ++"\n")
            else
                wrt string
        -- handle for analayis and Extraction ---  extract, analyse, extNorAnl
        handleForAnalAndExtract extAnal fileEnt deatilsLstRef dataTypRef wrt butType = do
            extnl <- readIORef extAnal
            let (button,isAnal) = butType
            case extnl of
                Nothing -> do
                -- Just _  -
                    --let senstize = G.widgetSetSensitive button True
                    --let deSenstize = G.widgetSetSensitive button False
                    let reSetFlag = do
                                        modifyIORef  extAnal (\_ -> Nothing)
                                        check <- readIORef extAnal
                                        print (show check)
                    filePath <- G.entryGetText fileEnt
                    case validateFilePath filePath of
                        Just errString  ->
                            runMsgDialog Nothing errString Error
                        Nothing  -> do
                            -- forkIO $ copty file to the files folder. Useful for extracting HMDIF
                            --(sl,_,_) <- readIORef detailsListRef
                            (sels,pms,_) <- readIORef deatilsLstRef
                            if null sels then
                                runMsgDialog Nothing "You have not selected any parameter investigate" Error
                            else do
                                -- parse the file for scrim or scanner
                                fileOk <-  readIORef dataTypRef >>= parseFile filePath -- fKind
                                case fileOk of
                                    Nothing -> do
                                    ----
                                        let line chr = '\n' : replicate 40 chr ++"\n"
                                        let toAnalyse (sc, (ye,dst,ns)) = display sc ++ ": \n\t-\
                                            \ Maint. frequency: "++show ye++"\
                                            \ years; \n\t- Dist. between maint. works: "++ show dst ++ "\
                                            \ Km; \n\t- Negative slopes: "++ show ns ++ line '-'
                                        let list = concatMap toAnalyse $ zip sels pms
                                        let st = "You have choosen to Analyse the following:"++ line '='
                                        let qt   = line '=' ++ "\n\nDo you wish to proceed? "
                                        ----- need a decision dialog here with the stuff being done
                                        let labelInfo = "Running MML2DS on the selected conditions. "
                                        --- set up the progress buffer ------------------------
                                        let runAnalysis = do
                                            g <- newStdGen
                                            let anF dp ye dst ns dk n m = exF dp ye dst ns dk n m
                                            --let anF tds = mml2DsGD g tds
                                            -- mml2DsGD g dp ye dst ns dk n m wrt
                                            --let exF _ _ _ _ _ _ _   = return (([],[]) :: ([(OModel, ([Double], [Int]))], [Int]))
                                            let analFun = if isAnal then anF else exF
                                            runAnalysisFromWindow filePath wrt analFun $ zip sels pms -- >> reSetFlag
                                        wrt labelInfo -- show what process is being done
                                        modifyIORef  extAnal (\_ -> Just isAnal)
                                        forkProcess (Just labelInfo) wrt runAnalysis (Just reSetFlag)
                                        -- deSenstize
                                    Just errString ->  runMsgDialog Nothing errString Error
                Just ea -> do
                    -- show a message if analysis or extract is clicked when a process is already running
                    let exOrAn = if ea then  "analysis"  else "extraction"
                    let errString = "An "++exOrAn++ " process is currently running, which \
                                    \ muct be completed before you can proceed with this action."
                    runMsgDialog Nothing errString Error
            where
                        exF :: Int -> Double -> Double -> Bool -> Int
                                -> Int -> Int -> [[Double]]
                                -> IO [([(OModel, ([Double], [Int]))], [Int])]
                        exF  _ _ _ _ _ _ _ _ = return []
                        ---


-------------------------------------------------------------------------------------
-- test: running the analysis on all the data from the analyisis window
-------------------------------------------------------------------------------------
type Data = [[Double]]
runAnalysisFromWindow :: String ->
    (String -> IO ()) ->
    --(Double -> Double -> Bool -> Data -> IO ([([(OModel, ([Double], [Int]))], [Int])])) ->
    (   Int    -> -- search depth
            Double -> -- range in years
            Double -> -- range in distance
            --g      -> -- random starting value
            Bool   -> -- ignore negative slopes
            --Int    -> -- the maximum number of possible cuts for the segment. See QueryDialog for details
            Int    -> -- the maximum number of chains to align
            Int    -> -- the minimum number of chains to align
            Int    -> -- the minimum number of points to trend
            [[Double]] ->
            IO [([(OModel, ([Double], [Int]))], [Int]) ]) ->
    [(Defect,(Double,Double,Bool))] -> IO ()
runAnalysisFromWindow fileName write2B anlF options = do
     let runAnalysis (sc ,(yr, ds, ns)) = -- (anlF yr ds ns) -- (\a -> anlF a yr ds ns)
            bs2HMDIF2bs_aux Nothing fileName write2B   Nothing Rapid (show sc, scode2align sc) (5,35) Nothing
     mapM_ runAnalysis options
     return () -- force a return

------------------------------------------------------------------------------------}
--
------------------------------------------------------------------------------------
validateEntries entries names =
    foldl' (\bs (v,st) ->
                --let st' = "maintenance works in "++ st
                if v >= 0 then bs
                else (Right ("The value entered for "++ st ++ " is not valid"))) (Left uevls) evals
    where evals     = zip (map entVal entries) names
          (uevls,_) = unzip evals
-------------------------------------------------------------------------------------
-- advanced options box for the display
--- tis shoulw be loaded from settings
advOptionsBox :: G.WindowClass window => window -> IO G.VBox
advOptionsBox win = do
    vbox <- G.vBoxNew False 4
    ----
    okButton <- G.buttonNewFromStock G.stockOk
    cancelButton <- G.buttonNewFromStock G.stockCancel
    bottomBox <- G.hBoxNew False 2
    sep <- G.hSeparatorNew
    mapM_ (\b -> G.boxPackEnd bottomBox b  G.PackNatural 2) [okButton,cancelButton]
    ----
    G.on cancelButton G.buttonActivated $ G.widgetDestroy win
    pms <- getInputPrms
    case pms of
        Right advpms -> do
            [sdEntry,aMxEnt, aMnEnt,mTrdEnt] <-  sequence $ replicate 4 G.entryNew
            let entries = [sdEntry,aMxEnt, aMnEnt,mTrdEnt]
            mapM_ (\ent -> do
                G.entrySetWidthChars ent 4
                G.entrySetMaxLength ent 4) entries
            let values = [searchDepth advpms, alignMax advpms, alignMin advpms, minTrend advpms]
            mapM (uncurry G.entrySetText) $ zip entries (map show values)
            let sdText = "Determines the number of refinements to parameters."
            let maxTxt = "The maximum number of adjacent chains to align."
            let minTxt = "The minimum number of adjacent chains to align."
            let trdTxt = "The minimum number of points to which a trend can be fitted."
            buttons <- mapM G.buttonNewWithLabel $ replicate 4 "What is This"
            mapM_ (uncurry onButtDo) $ zip buttons [sdText,maxTxt,minTxt,trdTxt]
            let strings = [" Search dept ", "Alignment maximum ","Alignment minimum ", "Trend minimum " ]
            otheBoxes <- mapM (\(str,w) ->  labelLeftofWidgetE (str++" ") 2  w True) $ zip strings buttons
            let hBoxes =  otheBoxes
            mapM_ (\(b,w) -> G.boxPackEnd b w G.PackNatural 2) $ zip hBoxes entries
            tableWithList <- tableWithWidgetList True hBoxes True 3
            G.boxPackStart vbox tableWithList G.PackNatural 4
            mapM_ (ioButtAct okButton advpms entries) [strings]
            ---
        Left str -> print str
    G.boxPackEnd vbox bottomBox G.PackNatural 0
    G.boxPackEnd vbox sep G.PackNatural 2
    return vbox
        where
            onButtDo butt str = G.on butt G.buttonActivated $ runMsgDialog Nothing str Info
            ioButtAct okButton advpms entries strings = do
              G.on okButton G.buttonActivated $ do
            --sdE <- G.entryGetText sdEntry [sdEntry,aMxEnt, aMnEnt,mTrdEnt]
                entered <- mapM G.entryGetText entries
                case validateEntries entered strings of
                    Left [ev,aMx,aMn,mTd] -> do
                        updateSDepth ev advpms
                        updateAlignMax aMx advpms
                        updateAlignMin aMn advpms
                        updateMinTrend mTd advpms
                    Right string -> runMsgDialog Nothing string Error  


-- advanced option window
advancedOptionsWin ::  IO  G.Window
advancedOptionsWin  =  do
     win <- G.windowNew
     opsBox <- advOptionsBox win
     G.set win [G.containerChild G.:= opsBox]
     G.widgetShow opsBox
     return win

--- dialog to edit the selected list
--drawingCanvas <-  getDrawingCanvas tabRef -- the drawing canvas --
-- openEditDialog :: [String] -> IO ([String], Bool)
openEditDialog comboRef detailsCombo detailsListRef inputBuffer  = do
    (selList,_,_) <- readIORef detailsListRef
    let len = length selList
    if len < 1 then
        runMsgDialog Nothing "There is nothing in the list to edit" Info
    else
        G.dialogNew >>= \dia -> do
            if len > 4 then G.widgetSetSizeRequest dia (-1) 250 else return ()
            deleteRef <- newIORef ([],True)
            table <- G.tableNew len 1 True
            -- add the table to a scrolled window
            selectedScrollWin <- vScrolledWinWithWgtList [table] VScroll
            -- pack stuff to the table let
            let selectedList = map display selList
            cbList <- mapM  G.checkButtonNewWithLabel  selectedList
            mapM_ (handleForCbuttons deleteRef) (zip selectedList cbList)
            let list = zip cbList [0..]
            mapM_ (\(bu, x) -> G.tableAttachDefaults table bu 0 1 x (x+1)) list
            -- add the scrolled window to the upper part of the dailog
            G.dialogGetUpper dia >>= \vbox -> do
                instructionLabel <- G.labelNew (Just " Select Conditions to remove  " )
                G.boxPackStart vbox instructionLabel G.PackNatural 3
                G.boxPackStart vbox selectedScrollWin G.PackGrow 2
            G.dialogGetActionArea dia >>= \hbox -> -- do
                mapM G.buttonNewWithLabel ["Select All", "Remove Selected"] >>=
                    \buttons -> do
                        G.on (buttons !! 0) G.buttonActivated $ do
                            mapM_ (\cb -> G.toggleButtonSetActive cb True) cbList
                        G.on (buttons !! 1) G.buttonActivated $ do
                            readIORef  deleteRef >>= \(toRemoveList,clear) -> do -- inputBuffer
                                if clear then do
                                    --mapM_ print toRemoveList
                                    if null toRemoveList then
                                        return () -- do nothing detailsListRef inputBuffer
                                    else do
                                        G.textBufferSetText inputBuffer ""
                                            --
                                        let selListNew  =  removeSel display toRemoveList selList
                                            --
                                        G.textBufferGetEndIter inputBuffer >>= \iter -> do
                                            let toBuffer = G.textBufferInsert inputBuffer iter
                                            mapM_ (\st -> toBuffer (display st ++"\n")) selListNew
                                        modifyIORef detailsListRef (\(_,xs,ys) -> (selListNew ,xs,ys))
                                --
                                else do
                                        G.textBufferSetText inputBuffer ""
                                        --clearComboList  comboRef detailsCombo
                                        modifyIORef detailsListRef (\(_,xs,ys) -> ([],xs,ys))
                            --
                            G.widgetDestroy dia
                        --
                        mapM_ (\b -> G.boxPackEnd hbox  b G.PackNatural 1) buttons
                --
            G.widgetShowAll dia
    where
        removeSel f [] ys     = ys
        removeSel f (x:xs) ys = removeSel f xs (filter ((x /=) . f ) ys)
        --
        handleForCbuttons delRef (string , cb) =
          G.on cb G.toggled $  do
            G.toggleButtonGetActive cb >>= \isSelected ->
                if isSelected then do
                    modifyIORef delRef (\(xs,_) -> (string : xs,True))
                else
                    modifyIORef delRef (\(xs,_) ->
                                    let ys = delete string xs in
                                        (ys, not $ null ys))

----------------------------------------------------------------------------------------------
-- set preview display
fillPreviewBox :: G.VBox -> PreviewParms  -> IORef [(String,DrawRef)] -> IO ()
fillPreviewBox pBox pprms drawRefList  = do
    --wrt get the top part: a horizontal box with two combo boxes and
    -- two buttons, as well as a radio button group to choose whether
    -- to view available schemes or sections
    optionsBox <-  G.hBoxNew False 0
    sep <- G.hSeparatorNew
    --[lframe ,rframe] <- sequence . replicate 2 $ G.hBoxNew False 2
    --[schmRButt, sectRButt] <- mapM G.radioButtonNewWithLabel ["Schemes","Sections"]
    --------- **** need handles for schmRButt and  sectRButt
    -- output these buttons in a vertical table
    --rButtstable <- tableWithWidgetList False [schmRButt, sectRButt] True 3
    --
    chainEntry <-  G.entryNew
    mapM_ (\ent -> do
                G.entrySetWidthChars ent 4
                G.entrySetMaxLength ent 4) [chainEntry]
    -- let buttLables = ["Add selection","Show subsections","Show all","Analyse selected subsections"]
    let buttLables = ["Add selection","Show subsections","Add all","Analyse selected subsections"]
    [lAnalButt, addButt, remButt,stackButt] <-  mapM G.buttonNewWithLabel buttLables
    --                         flip (horizontalWidgetList True) False
    G.widgetSetSensitive optionsBox False
    --G.containerAdd rframe rbox
    sectsRef <- newIORef [] -- an IOref for defects to be used by the clearAdd function
    drawRef <- newIORef emptyDrawRwf -- emptyDrawRwf
    indexRef <- newIORef [] -- ^ the indexes in the notebook
    --  HANDLE FOR STAKING -----------
    G.on stackButt G.buttonActivated $ do
        drf <- readIORef drawRef
        let ln = length ( chnLabel drf)
        addedChains <- G.entryGetText chainEntry >>= return . entVal
        if all (> 0) [ln , addedChains] then  do
            let name = caption drf
            let fromTo from to = myTake to . drop from
            let currData = fromTo (currChain drf) (addChain drf) (chnLabel drf)
            let range = " chains" ++ (head currData) ++ " to " ++ last currData
            wrtFun pprms (name ++ range)
        else
            return () -- a message here?
    --avlLabel <- G.labelNew (Just "Available conditions/schemes: ")
    [avlCombo,sectionsCombo] <- sequence $ replicate 2 G.comboBoxNewText
    ----- add options to the avlCombo
    ----- get the list for avlCombo by checking to see what ahs been extracted
    ----- The conditions alaready extracted, but not necessarily, analysed, are added to the list
    ----- The same is done for available schemes
    --let extract = (checkIfExtracted (\_ -> return ())) `seq` getAlreadyExtracted
    getAlreadyExtracted True >>= mapM_ (G.comboBoxAppendText avlCombo)
    G.on avlCombo G.changed $ handleForCondComboSelection sectsRef avlCombo sectionsCombo
    avlBox <- labelLeftofWidget "Extracted: " avlCombo False

    G.widgetSetSizeRequest sectionsCombo 130 (-1)
    sectBox <- labelLeftofWidget "Sections: " sectionsCombo False
    -- now pack the boxes at the top
    comboBoxes <- tableWithWidgetList False [avlBox,sectBox] False 4
    G.set comboBoxes [G.tableChildXPadding avlBox G.:= 2]
    -- now pack the bottom bit, omitting the external option for the time being
    mapM_ (\w -> G.boxPackEnd optionsBox w G.PackNatural 3) [lAnalButt,stackButt]
    G.boxPackStart optionsBox chainEntry G.PackNatural 2
    --G.boxPackStart topBox sectBox G.PackGrow 3
    mapM_ (\t -> G.boxPackStart optionsBox t G.PackNatural 2) [addButt, remButt]
    ---
    --------------------------------------------------------
    ------ now get the bottom notebook
    (_,ntbk) <- notebookWithScrollWin False ([] :: [(G.VBox,String)]) False
    -- now handle selection of sections using, placing the chains in the notebook
    let sensAndHandles = (optionsBox,chainEntry,addButt, remButt,lAnalButt )
    G.on sectionsCombo G.changed $ do
        sectComboHandle ntbk indexRef drawRef avlCombo sectionsCombo sensAndHandles
    -- handle for local analysis
    --G.on lAnalButt G.buttonActivated $ lAnalButtHandle drawRef pprms True
    G.on stackButt G.buttonActivated $ lAnalButtHandle drawRef pprms False
    --------------------------------------------------------------------------------------
    --      packing into the box for display
    G.boxPackStart pBox  comboBoxes G.PackNatural 3
    G.boxPackStart pBox  optionsBox G.PackNatural 3
    G.boxPackStart pBox sep G.PackNatural 0
    G.boxPackStart pBox ntbk G.PackGrow 0
    G.widgetShowAll pBox
    where
        handleForCondComboSelection cRef condCombo sectsCombo = do
            -- will need to put options for sections or scehmes here
            mtext <- G.comboBoxGetActiveText condCombo
            case mtext of
                Just text -> do
                    -- add options to the sections combo
                    let defectCR = takeWhile (/= ' ') text
                    let outfile = "./Defects/"++"out"++ defectCR ++".txt"
                    fileExists <- doesFileExist outfile
                    if fileExists then do
                        -- let extractSects    =  map (snd . break isDigit . fst)
                        --let extractSections =  do
                        getSectionsAndSorrogateS outfile >>= clearAdd cRef sectsCombo
                    else do
                        let string = "Data for "++ defectCR ++ " have not been \
                              \extracted. Would you like to extact them now?"
                        --let fileName = (show defect) ++".txt"
                        runDecisionMessageWindow Nothing string (return ())
                Nothing -> return ()
        -- handle for sections combos (need to update the notebook here)
        sectComboHandle ntbk indRef dref avlCombo sectionsCombo opts = do
            --let title = maybe "" id $G.comboBoxGetActiveText avlCombo
            defectCR <- G.comboBoxGetActiveText avlCombo >>= return . maybe "" (takeWhile isAlphaNum)
            let (optsBox,chnEnt,addButt, remButt,lAnalButt ) = opts
            let outFile = "./Defects/"++"out"++ defectCR ++".txt"
            validOption <- doesFileExist outFile
            if validOption then do
                --print "enter valid file"
                mtext <- G.comboBoxGetActiveText sectionsCombo
                case mtext of
                    Just text -> do
                        canvas <- getDrawingCanvas (Just dref)
                        ---------- sensetise and respond to ---------------------
                        G.widgetSetSensitive optsBox True
                        -- handle for add chain
                        G.on addButt G.buttonActivated $ do
                                G.entryGetText chnEnt >>= \s ->
                                    modifyIORef dref (updateAddChain True $ entVal s)
                                G.widgetQueueDraw canvas
                        -- handle for remove chain
                        G.on remButt G.buttonActivated $ do
                                G.entryGetText chnEnt >>= \s ->
                                    modifyIORef dref (updateAddChain False $ entVal s)
                                G.widgetQueueDraw canvas
                        ----------------------------------------------------------

                        let (snm, srg) = span isAlphaNum text
                        let sectSrg' = words srg -- dropWhile (not . isDigit) srg
                        let srg = if length sectSrg' <= 1 then "" else (last sectSrg')
                        --chD <- getSectionData sname srg outFile -- >>= \ -> do -- liftM unzip .
                        (bCD,dds) <- getSectionData snm srg outFile >>= liftM unzip . mapMPair G.buttonNewWithLabel
                        modifyIORef dref (updateDrawData dds)
                        mapM G.buttonGetLabel bCD >>= modifyIORef dref . curryRev updatePtsLabel
                        let title  = defectCR ++ ": "++text
                        modifyIORef dref (updateCaption title)
                        mapM_ (updatePlot canvas dref) (zip bCD [0 .. ])
                        --
                        let ff = onSwith dref
                        G.hBoxNew False 0 >>= add2nbk indRef title ntbk canvas ff dref bCD -- butts
                        --print "not boos houls have been added"
                    Nothing -> print "No selection recorded for sectionCombo"
            else  return ()
            where
                ---
                curryRev :: (a -> b -> c) -> (a -> (b -> c))
                curryRev f  a = f a
                -- actions to do  on a swith page or a delete
                onSwith  ref i  =  do -- switch
                                   dlist <- readIORef drawRefList
                                   modifyIORef ref (\_ -> snd (dlist !! i))
                                   readIORef drawRefList >>= print . show . length
                                   --print ("page: " ++ show i)
                --
                mapMPair :: Monad m => (a -> m b) -> [(a,c)] -> m [(b,c)]
                mapMPair  f xs = sequence [liftM2 (,) (f a) (return c)  | (a,c) <- xs ]
                -- update the values to plot
                updatePlot canvas dref (button, index) =
                    G.on button G.buttonActivated $ do
                        modifyIORef dref (updateCurrChain index)
                        updateDrawingArea canvas dref >> return ()
                --
                setDispFrame widget frame =
                    G.set frame [G.containerBorderWidth G.:= 0, G.frameLabelYAlign G.:= 0.5,
                                G.frameLabelXAlign G.:= 0.5,G.containerChild G.:= widget]
                --
                add2nbk indexRef tabTitle  noteb  canvas ff dref buttons rHBox = do
                    lHBox <-  G.hBoxNew False 0
                    --lVBox <-  G.vBoxNew False 1
                    [chainsFrame,groupsFrame] <- sequence $ replicate 2 G.frameNew
                    vsep   <- G.vSeparatorNew
                    if null buttons then
                        -- message here that there is nothing to add
                        return ()
                    else do
                        buttSclArea <- vScrolledWinWithWgtList buttons VScroll
                        ---
                        G.boxPackStart lHBox buttSclArea G.PackNatural 0
                        G.boxPackStart lHBox vsep G.PackNatural 0
                        G.boxPackEnd lHBox canvas G.PackGrow 1
                        ---
                        let fs = filter ((/= tabTitle) . fst)
                        readIORef dref >>= \dr -> modifyIORef drawRefList ((++ [(tabTitle,dr)]) . fs)
                        ---
                        setDispFrame lHBox chainsFrame
                        --- note the results go in rbox
                        --print "notebook should be added"
                        setDispFrame rHBox groupsFrame
                        hPanedArea  groupsFrame chainsFrame 0 >>=
                            noteBookAddWidget noteb tabTitle indexRef (Just drawRefList) ff
                        G.widgetQueueDraw canvas
        --- handle for local analysis
        lAnalButtHandle :: IORef DrawRef -> PreviewParms -> Bool -> IO ()
        lAnalButtHandle drwRef pprms allSect = do
            [str1,str2] <- mapM G.entryGetText [yrsEnt pprms, distEnt pprms]
            let prt  = wrtFun pprms
            let fromTo from to = myTake to . drop from
            let datYrs a b = if allSect then unzip . map unzip else unzip . map unzip . fromTo a b
            negSlope <- G.toggleButtonGetActive (nSpopes pprms)
            case (validateEntries [str1,str2] ["years","distance"]) of
                Left [e1,e2] -> do
                    -- do the analysis on the selected block
                    dr <- readIORef drwRef
                    --let     n       = lengt (chnLabel dr)
                    let (yr,dst,ns) = (fromIntegral e1, fromIntegral e2,negSlope)
                    let (yrs, dat)  = datYrs (currChain dr) (addChain dr) (drawData dr)
                    let chns        = if allSect then (chnLabel dr) else fromTo (currChain dr) (addChain dr) (chnLabel dr)
                    let title       = caption dr
                    let title'      = filter isAlphaNum title
                    let jt          = Just  ("Analysing "++ title)
                    jtt    <- G.labelNew jt
                    let writr = G.labelSetText jtt
                    let anal        = analyseData drwRef yr  dst ns prt dat yrs chns title'
                    let anal'       = forkProcessWithScroll (Just jtt) writr anal Nothing
                    proceedAfterConsent negSlope title (anal' >> return ())
                Right string ->
                    runMsgDialog Nothing string Error
            where
                ---- need to remove the analysis (should only
                analyseData drwRef yr dist nslp pf daTa yrs chains title = do
                    inputParms <- getInputPrms
                    case inputParms of
                        Right  iprs -> do
                            g <- newStdGen
                            let dpt =  searchDepth iprs     -- search depth
                            let aMx = alignMax iprs     -- maximum number of chains to align
                            let aMn = alignMin iprs     -- minimum number of chains to align
                            let mTd = minTrend  iprs      -- minimum number of points to trend
                            --- use a depth of 5 instead of in excess of 100 for more powerful machines
                            let anl _ = return [] -- mml2DsGD g 100 yr dist (not nslp) aMx aMn mTd pf
                            let resultDir = "./LocalAnalyais"
                            let chnn = if null chains then "" else takeWhile (/= '-') (head chains) ++"--" ++ takeWhile (/= '-') (last chains)
                            let resultFile = resultDir++('/' : title)++".txt"
                            let resultRecFile = resultDir++"/schemeResults.txt"
                            let chainsHeader = L.pack "chains:"-- L.pack "-------CHAINS---------\n"
                            let yearsHeader = L.pack "years:"-- L.pack "-------YEARS---------\n"
                            let rHdr        = L.pack "-------RESULTS--------\n"
                            let chans     = chainsHeader <> (L.pack (printElm chains)) <> (L.pack "\n")
                            --let yrsSS    = L.intercalate (L.pack "\n") $ map (L.pack . printElm) yrs
                            let yrsSS  = (L.pack . printElm) (take 1 yrs)
                            let yrss     = yearsHeader <> yrsSS <> (L.pack "\n")
                            --- debug "./LocalAnalyais/schemeResults.txt"
                            let dataVals = L.pack (printElm $ concat daTa)
                            let dataa = (L.pack "data:") <> dataVals <> (L.pack "\n")
                            print (show $ concat daTa)
                            --print $ show [show dpt, show yr, show dist, show nslp, show aMx, show aMn, show mTd ]
                            -- calculat the results and logg it ----(rHdr <>) .
                            yn <- doesDirectoryExist resultDir
                            if yn then return () else createDirectory resultDir
                            let !prtResults =  ((chans <> yrss) <>) . (dataa <> ) . results2BS'
                            --let chnn = if null chains then "" else takeWhile (/= '-') (head chains) ++"--" ++ takeWhile (/= '-') (last chains)
                            -- print (show anl)
                            anl daTa >>= \rlt -> do
                                (L.writeFile resultFile .  prtResults) rlt
                                (modifyIORef drwRef . omod2derf) rlt
                                let record = L.snoc (L.pack (title )) '\n' --- ++":"++chnn
                                L.appendFile resultRecFile record
                                --addSchemeResults (L.pack title) resultRecFile
                            -- print . show --  mapM_ (L.writeFile resultFile .  prtResults)
                        Left string    -> print ("error reading parameter file: "++string)
                        --- infp for dialog
                proceedAfterConsent ng capt anal = do
                    let mainStr = "You have chosen to analyse "++capt
                    let endStr  = if ng then
                                     ", and have opted to include negative slopes. Are\
                                     \ you sure you want to include negative slopes?"
                                  else
                                     ". Do you wish to proceed?"
                    runDecisionMessageWindow Nothing (mainStr++endStr) anal
                ---
                omod2derf :: [([(OModel, ([Double], [Int]))], [Int])] -> DrawRef  -> DrawRef
                omod2derf   xs  = updateResults rr
                    where ks = concatMap (concatMap (fst . snd) . fst)  xs
                          rr = Results 0 0 0 [] 0 ks []
                --
                addSchemeResults :: L.ByteString -> FilePath -> IO ()
                addSchemeResults result fileName = do
                    -- have to change the result filename so it reflects the chains in the schemeas well
                    schmResults <- L.readFile fileName >>= return . L.lines
                    print "got here"
                    if any (== result) schmResults then
                        return ()
                    else
                        L.appendFile  fileName result
----------------------------------------------------------------------------------------------
--      End of Network Level Dialog Construction and related windows
----------------------------------------------------------------------------------------------}
