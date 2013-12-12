{-# LANGUAGE BangPatterns #-}
-- test menu and toolbars
-- the main window

import qualified Graphics.UI.Gtk as G
-- import qualified Graphics.UI.Gtk.Cairo as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IORef
import Data.List (sort,find,delete,foldl', delete,findIndex, nub)
import Data.Maybe (maybe, fromJust, isJust, listToMaybe)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM) -- ,liftM2)
import Control.Concurrent (forkIO,killThread)
import Control.Exception (SomeException)
import System.IO.Unsafe (unsafePerformIO)
--import Control.Concurrent () -- ,forkIO,ThreadId)
import Char (toUpper,isDigit,isSpace)
import Control.Exception as E (catch,IOException)
import System.Directory (doesFileExist)
--import System.Directory (doesFileExist, doesDirectoryExist,createDirectory,
---------------------------------------------------------------------------------------------------
import ProcessFile
import Parallel (forkProcess,forkProcessWithScroll,processWithScroll,myTry,forkProcessWithID)
import NiceFork
import GlobalParameters
import Displays (mkDisplayBox)
import DrawingFrame --(openProgessWindow,updateDrawingArea)
import HMDIFPrelude (conditions,splitAtChunkBS,bs2Int',bsReadChr)
import InterfaceQAWindows
import FileIO (checkIfExtracted,removeExtracted,csv2HMDIF_aux)
import ReadFromDataFiles2  (writeInputPrms,getInputPrms,InputPrms(InputPrms) , bs2HMDIF2bs_aux)
import RprGD2 (OptimisationType(..))
--
import ControlFlow (makeAnalViewInterface, convertSimFileWindow, ResultType (..),getRestriction) --- ,WindowTypes (..) ,visualize,optionsWindow ,extractWindow)

---------------------------------------------------------------------------------------------------
-- paths
hmdifPath = "./SimulationFiles/hmdifPath.txt"
scannerPath = "./SimulationFiles/scannerPath.txt"
scrimPath = "./SimulationFiles/scrimPath.txt"
csvPath = "./SimulationFiles/csvPath.txt"
--
resultsFile = "./SectionResults/results.txt"
logoPath = "./Logos/nottDevonLogo.png"
roadNamesPath = "./SimulationFiles/roadNamesPath.txt"
-- scannerPath, scrimPath,csvPath,roadNamesPath
-----------------------------------------------------------------------------------------------------
-- create an area at the right to show the progress log
progressLogArea :: IO (G.Notebook,[G.TextBuffer])
progressLogArea = do
    -- logArea <- getDrawingCanvas Nothing
    buffers <- sequence . replicate 2 $ G.textBufferNew Nothing
    textViews <- mapM setIter buffers
    let names = ["Logs", "Processes"]
    (_,nbk) <- notebookWithScrollWin True (zip textViews names) False
    return (nbk, buffers)
    where
        setIter tbuffer = do
            -- mark <- G.textMarkNew Nothing False
            tview <- G.textViewNewWithBuffer tbuffer
            G.set tview [G.widgetCanFocus G.:= False, G.textViewWrapMode G.:= G.WrapWord,
                         G.textViewEditable G.:= True]
            return tview

-- home screen footer: a label with information about the date and time
homeScreenFooter :: IO G.Label
homeScreenFooter =    G.labelNew (Just "Footer")
---------------------------------------------------------------------------------------------------
---- custom stockItems
profileStockItem = G.StockItem "stockProfile, " "Profile" [G.Button1] (G.keyFromName "P") ""

-- get main window contents
getImage = do
    ---- get DCC image
    hbox <- G.hBoxNew False 0
    vbox <- G.vBoxNew False 1
    image <-  G.imageNewFromFile  logoPath -- (-1) 100 "./Logos/nottDevonLogo.png" 400 150
    G.widgetSetSizeRequest hbox (-1) 240 -- 15  (-1)
    G.set hbox [G.containerChild G.:= image]
    G.boxPackStart vbox hbox G.PackNatural 3
    return vbox

--------------- adding note books for each option clicked----------------------------------------
loadChecks :: (String -> IO ()) -> IO ()
loadChecks  log = do
    let paths = [scannerPath, scrimPath,csvPath,roadNamesPath]
    let pathNames = ["scannerPath", "scrimPath","csvPath","roadNamesPath"]
    mapM_ (uncurry $ logPathExist log) $ zip paths pathNames
    where
        -- log path exist
        logPathExist :: (String -> IO ()) -> FilePath -> String ->  IO ()
        logPathExist loG path pathName  = do
            exist <- getFileAtPath path
            if exist then
                loG (pathName ++ " found OK ..")
            else
                loG ("No " ++ pathName ++ " loaded ...")
        --
        getFileAtPath :: FilePath -> IO Bool
        getFileAtPath path = do
            exist <- doesFileExist path
            --{-
            if exist then  do
                contents <- bsReadChr ',' path
                if null contents then
                    return False
                else do
                    let hs = head contents
                    if null hs then
                        return False
                    else if length hs == 1 then
                        doesFileExist $ L.unpack (hs !! 0)
                    else
                        doesFileExist $ L.unpack (hs !! 1)
            else
            --}
                return exist
-----------------------------------------------------------------------------------------------}
-- runMsgDialogc -
checkForDefaultParms = do
    let defaultParms = InputPrms 85 10 2 6
    mprma <- getInputPrms
    case mprma of
        Left _   -> writeInputPrms defaultParms
        Right _ ->  return ()

-- sort results file
fresultsFile :: (String -> IO()) -> IO ()
fresultsFile prt = do
    let resultsFile = "./SectionResults/results.txt"
    exists <- doesFileExist resultsFile
    if exists then do
        !lists <- L.readFile resultsFile >>=  return . nub  . L.lines
        L.writeFile resultsFile $ L.intercalate (L.pack "\n") lists
        prt (show (length lists) ++ " conditions analysed so far ...")
    else
        prt "No condition analysed fo far"

main :: IO ()
main = do
    G.unsafeInitGUIForThreadedRTS --
    --- removes the previous extracted file
    introWin <- openStartupWindow =<< mainWindow
    G.widgetShowAll introWin -- =<< mainWindow showLogPane
    G.mainGUI

-- VIEWA

uiDecl =  "<ui>\
\           <menubar>\
\            <menu action=\"FMA\">\
\              <menuitem action=\"OPENA\" />\
\              <separator />\
\              <menuitem action=\"EXIA\" />\
\            </menu>\
\            <menu action=\"ANA\">\
\              <separator />\
\              <menuitem action=\"NTWKA\"/>\
\              <menuitem action=\"SIMMA\"/>\
\            </menu>\
\            <menu action=\"VIEWA\">\
\            </menu>\
\            <menu action=\"HMA\">\
\              <menuitem action=\"MANA\" />\
\              <menuitem action=\"ABOUTA\" />\
\            </menu>\
\           </menubar>\
\           <toolbar>\
\            <toolitem action=\"OPENA\" />\
\            <separator />\
\           </toolbar>\
\          </ui>" -- </pre>"
--- the main window ------
mainWindow ::  IO G.Window
mainWindow  = do
    ---------------------------------------- main Window ------------------ openStartupWindow
    window <- G.windowNew
    --
    G.set window [G.windowTitle G.:= "Pavement Data Preparation and Analysis",
                  G.windowDefaultWidth G.:= 850, G.windowDefaultHeight G.:= 400]
    G.windowMaximize window
    chainsRef <- newIORef []
    --- check and load extracted files
    --
    G.stockAddItem [profileStockItem]
    --- cretare  the main interface
    [mainBox, topBox] <- sequence . replicate 2 $ G.vBoxNew False 0
    -- topBox <- G.hBoxNew False 0
    (nbk, buffers) <- progressLogArea
    forkIO $ do
        let writer = (updateBufferAtEnd (buffers !! 0))
        loadChecks writer
        setRoadClasses writer --  schemeRec2Name
        -- fresultsFile writer
    eparea <- vPanedArea topBox nbk 200
    footer <- homeScreenFooter
    G.boxPackStart mainBox eparea G.PackGrow 0
    G.boxPackEnd mainBox footer G.PackNatural 0
    G.containerAdd window  mainBox -- eparea --
    ----
    ------------- set up the menu
    fma <- G.actionNew "FMA" "File" Nothing Nothing
    --draw <- G.actionNew "DRAW" "POptions" Nothing Nothing
    hma <- G.actionNew "HMA" "Help" Nothing Nothing
    anl <- G.actionNew "ANA" "Analyse" Nothing Nothing
    view <- G.actionNew "VIEWA" "Results" Nothing Nothing

    mana <- G.actionNew "MANA" "Manual" (Just "User Manual") Nothing
    ------
    -- dna <- G.actionNew "DNA" "DNA Plot" (Just "Get DNA Plot") (Just G.stockSelectColor)
    ------
    let scannerHelp = "Analyse DATA on Rutting, Texture, Cracking, Profile and Edge Condition "
    ntwka <- G.actionNew "NTWKA" "Analyse SCANNER or SCRIM Data"  (Just scannerHelp) Nothing -- (Just G.stockProperties)
    smlka <- G.actionNew "SIMMA" "Analyse CSV Data"  (Just scannerHelp) Nothing
    exia <- G.actionNew "EXIA" "Quit"    (Just "Exit the program") (Just G.stockQuit)
    --- file actions
    opena <- G.actionNew "OPENA" "Update Source Files" (Just "Load a new source HMDIF file") Nothing
    --exrta <- G.actionNew "EXRTA" "Extract Conditon" (Just "Extract condition from HMDIF  file") Nothing
    saveAsa <- G.actionNew "SAVEASA" "Save As" Nothing (Just G.stockSaveAs)
    printa <- G.actionNew "PRINTA" "Visualization" (Just "Visualize Data")(Just G.stockPrint)
    --- "OPENA" "SAVEA" "SAVEASA" "PRINTA"
    hlpa <- G.actionNew "ABOUTA" "About"  (Just "About the Program") (Just G.stockAbout)
    agr <- G.actionGroupNew "AGR"
    --drawG <- G.actionGroupNew "DRAWGROUP"
    mapM_ (G.actionGroupAddAction agr) [fma, hma,anl, view] -- draw,
    mapM_ (\act -> G.actionGroupAddActionWithAccel agr act Nothing)
           [mana,hlpa,ntwka,smlka, -- ntra,
          --dna,ruta,texa,profa,crka,multa,rema,erra,fcsta,
            opena] -- ,saveAsa,printa]
    --mapM_ (G.actionGroupAddAction drawG) [dna]
    G.actionGroupAddActionWithAccel agr exia (Just "<Control>e")
    --
    ui <- G.uiManagerNew
    G.uiManagerAddUiFromString ui uiDecl
    G.uiManagerInsertActionGroup ui agr 0
    --
    maybeMenubar <- G.uiManagerGetWidget ui "/ui/menubar"
    let menubar = case maybeMenubar of
                       (Just x) -> x
                       Nothing -> error "Cannot get menubar from string."
    --- put widgets at the top of the topBox
    G.boxPackStart topBox menubar G.PackNatural 0 -- packing the menu bar
    image <- getImage
    G.boxPackStart topBox image G.PackGrow 0
    ----------
    maybeToolbar <- G.uiManagerGetWidget ui "/ui/toolbar"
    let toolbar = case maybeToolbar of
                       (Just x) -> x
                       Nothing  -> error "Cannot get toolbar from string."
    -- add the tool bar
    --G.boxPackStart mainBox toolbar G.PackNatural 0 -- packing the tool bar

    ---------deactivate some options --
    --let des = [scra, acca] -- erra,multa,rema,
    --mapM_ (\a -> G.actionSetSensitive a False) des
    ----------------------------------------------------------------------------------------------
    -- handles for the action buttons and menu options --
    G.onActionActivate hlpa runAboutDialog
    --G.onActionActivate mana openFeedBackForm -- openVisualizationWindow
    G.onActionActivate ntwka $  handleAnanlysysOpt "SSCRIM" buffers -- maybe convertSimFileWindow myDialogShow =<< makeAnalViewInterface buffers True
    --optionsWindow Nothing AnalyseOrExtract  >>= showOrError -- do simulations analysis here
    G.onActionActivate smlka $ handleAnanlysysOpt "CSV" buffers -- analyseSimFileWindow  buffers -- analyseSimFileWindow
    -- G.onActionActivate schma $ optionsWindow SchemAnalysis  >>= showOrError
    G.onActionActivate opena $  do
                                   let wrtr str = G.postGUIAsync $ updateBufferAtEnd (buffers !! 0) str
                                   convertSimFileWindow wrtr-- updateSourceFiles
    ----
    G.onActionActivate view $   openVisualizationWindow -- visualize NScanner >>= G.widgetShowAll --
    --
    G.onActionActivate exia $
        runDecisionMessageWindow Nothing "Are you sure you want to exit?" (G.widgetDestroy window)
    ----------------------------------------------------------------------------------------
    let shutDown =  G.mainQuit -- shut down shold include writing to the log file ?
    G.onDestroy window shutDown
    G.onDelete window (\_ ->  shutDown >> return True )
    return window
        where
            showOrError ewin =
                case ewin of
                    Left win  -> G.widgetShowAll win
                    Right str -> runMsgDialog  Nothing str  Error


-- data ResultType  = CSV | SScanner | Accident | SScrim
handleAnanlysysOpt ::  String -> [G.TextBuffer] ->  IO ()
handleAnanlysysOpt rtype buffers
    | rtype == "CSV" = do
            exists <- doesFileExist (paths !! 0)
            if exists then do
                L.writeFile (paths !! 1) =<< L.readFile (paths !! 0)
                analyseSimFileWindow  buffers
            else do
                -- message that the csv path does not exist and prompt to laod a csv path
                let string = "No CSV file is loaded. Would you like to load a CSV file?"
                runDecisionMessageWindow Nothing string $ convertSimFileWindow wrtr
                --runMsgDialog  Nothing string  Error
    | rtype == "SSCRIM" = do

            maybe (convertSimFileWindow wrtr) myDialogShow =<< makeAnalViewInterface buffers True
    | otherwise = return ()
        where
            wrtr str = G.postGUIAsync $ updateBufferAtEnd (buffers !! 0) str
            paths  = ["./SimulationFiles/csvPath.txt"
                      , "./SimulationFiles/hmdifpath.txt"]
--}


 -- open visualization window
openVisualizationWindow :: IO ()
openVisualizationWindow  = do
    exists <- doesFileExist resultsFile
    if (not exists) then
        runMsgDialog (Just "No results ") "There are no analysis results currently available to view."  Error
    else do
        win <- G.windowNew
        G.set win [G.windowTitle G.:= "Visualizations", G.windowResizable G.:= True,
                   G.windowDefaultWidth G.:= 900, G.windowDefaultHeight G.:= 600]
        vbox <- mkDisplayBox resultsFile
        -- topViewBox win "Analalysis Options " >>= \vbox ->  do
        G.set win [G.containerChild G.:= vbox]
        G.widgetShowAll vbox
        --openAnalWindow >>= G.widgetShowAll win
        G.widgetShowAll win
---------------------------------------------------------------------------------------------------

----------- analysis window ---
analyseSimFileWindow :: [G.TextBuffer] -> IO ()
analyseSimFileWindow buffs = do
    win <- myDialogNew
    G.set (myDia win) [G.windowTitle G.:= "Analyse Simulation File", G.windowResizable G.:= True]
    entries <- sequence $ replicate 4 G.entryNew
    mapM_ (\ent -> do
                G.entrySetWidthChars ent 6
                G.entrySetMaxLength ent 6) entries -- [yearEntry, distEntry, depthEntry,limEntry]
    ---
    let labelEnt a b = labelLeftofWidgetE a (6 * length a) b  True
    let strList = ["Enter distance between maint. interventions points: ",
                   "Enter years between maint. interventions: ",
                   "Enter search depth: ","Change seed number:"] -- Enter number to limit entry file: "]
    ---
    [dbox, ybox,dptbox,seedEnt] <- mapM (uncurry labelEnt) (zip strList entries)
    --mapM_ (flip G.widgetSetSensitive False) (map snd schms)
    let [yearEntry, distEntry, depthEntry,limEntry] = entries
    G.widgetSetSensitive dptbox False
    let optimisations = ["Optimise from data: ", "Rapid selection: ", "Optimise with depth: "]
    dummy <- G.radioButtonNew
    radios <- mapM (G.radioButtonNewWithLabelFromWidget dummy) optimisations
    let withDepth = radios !! 2
    let fromData  = radios !! 0
    ckButtons   <- makeRadioButtonGroupFromStrings  ["","Restrain slopes to positive  ",
                                                     "Restrain slopes to negative",
                                                     "Unrestrained slopes"]
    --
    let mixfile = "./mmlTdsFiles/mixture.txt"
    let setRst (b,n) = G.on b G.toggled $ do
                            isTogg <- G.toggleButtonGetActive b
                            if isTogg then L.writeFile mixfile (L.pack $ show n) else return ()
    ---
    mapM_ setRst $ zip (tail ckButtons) [0,1 .. ]
    --
    let buttonNames = [" Analyse ", " Cancel "]
    [analyseButt, cancelButt]  <- mapM G.buttonNewWithLabel  buttonNames
    ---
    --- tableWithWidgetList :: G.WidgetClass widget => Bool -> [widget] -> Bool -> Int -> IO G.Table
    topTab <- tableWithWidgetList True  [ybox,dbox] False 2 -- ,limBox
    frame <- G.frameNew
    G.frameSetLabel frame "Optimisations for group formations"
    G.vBoxNew False 3  >>= \vbox -> do
        G.containerAdd frame vbox
        --
        hbox <- G.hBoxNew False 3
        rBox <- G.vBoxNew False 1
        sep <- G.vSeparatorNew
        radiosTab <- tableWithWidgetList False  (init radios) False 5
        mapM_ (\bt -> G.boxPackStart vbox bt G.PackNatural 5) (tail ckButtons)
    ---
    myDialogAddWidget win topTab
    myDialogAddWidget win frame
    --myDialogAddWidget win sbox
    -----------------------------
    mapM_ (myDialogAddButton win) [cancelButt,analyseButt]
    ---
    G.on analyseButt G.buttonActivated $ do
        -- let [yearEntry, distEntry, depthEntry,limEntry] = entries
        ents <- mapM (liftM (bs2Int' . L.pack) . G.entryGetText) entries
        let (n,m) = (ents !! 0 , ents !! 1)
        case maybe2 n m of
            Just (year,dist) ->
                if (year > 0 && dist > 0) then  do
                    let yDist  = (fromIntegral year, fromIntegral dist)
                    getDepth <- G.toggleButtonGetActive withDepth
                    if getDepth then  do
                        --- mdpt <-  (liftM (bs2Int' . L.pack) . G.entryGetText) depthEntry
                        case (ents !! 2)  of
                            Nothing -> runMsgDialog  Nothing "The search depth is invalid"  Error
                            Just dpt ->
                                if dpt < 0 then
                                    runMsgDialog  Nothing "Please enter a positive value for the search depth"  Error
                                else do
                                     myTry $ analyseWithOptimisation (myDia win) buffs ents ckButtons (False, Just dpt) yDist
                    else do
                        -- get the parameter
                        opts <- mapM G.toggleButtonGetActive $ init radios
                        if (opts !! 0) then
                            myTry $ analyseWithOptimisation (myDia win) buffs ents ckButtons (False, Nothing) yDist
                            --print "Optimise from data: "
                        else
                            myTry $ analyseWithOptimisation (myDia win) buffs ents ckButtons (True, Nothing) yDist
                            --print "Rapid selection: "

                else if year > 0 then
                    runMsgDialog  Nothing "Please enter a positive number for distance " Error
                else
                    runMsgDialog  Nothing  "Please enter a positive value for years " Error
            Nothing ->
                if (isJust n) then
                    runMsgDialog  Nothing  "The value entered for distance is invalid " Error
                 else
                    runMsgDialog  Nothing  "The value entered for years is invalid " Error
    ---
    G.on cancelButt G.buttonActivated $ G.widgetDestroy (myDia win)
    myDialogShow win
    where
        analyseWithOptimisation :: G.Window -> [G.TextBuffer] -> [Maybe Int] -> [G.RadioButton]  -> (Bool,Maybe Int) -> (Double, Double) -> IO ()
        analyseWithOptimisation win buffers ents ckButtons (raPid,mDepth) yDist = do
            ---- first check options for restraining slopes
            restr <- liftM (maybe [] (\_ -> "1") . listToMaybe . filter (== True)) $
                                mapM G.toggleButtonGetActive (tail ckButtons)
            if null restr then  do
                let msg = " You did not enter an option for restraining slopes "
                runMsgDialog  (Just "Missing Options") msg Error
            -- check that a restrain option was selected
            else
                analyseWithOptimisation_aux win buffers ents ckButtons (raPid,mDepth) yDist
        -- do the analysis
        analyseWithOptimisation_aux win buffers ents ckButtons (raPid,mDepth) yDist = do
            let mkOpt   = if raPid then Rapid else FromData
            let optType = maybe mkOpt WithDepth mDepth
            --- include mixture model or not --
            mPath <- liftM (listToMaybe . L.lines ) $ L.readFile hmdifPath
            case mPath of
                Nothing -> runMsgDialog  Nothing  "Error reading HMDIF source path " Error
                Just bs -> do
                    G.widgetDestroy win
                    let (deft, pth)   = splitAtChunkBS (L.pack ",") bs
                    let path          = L.unpack pth
                    -- print ("path is: "++ path)
                    pathExist   <- doesFileExist path
                    if pathExist then do
                        let defect        = L.unpack deft
                        -- let [b1,b2]  = buffersl
                        labl <- G.labelNew Nothing
                        restr <- getRestriction
                        let wrt string     =  G.postGUIAsync $ G.labelSetText labl string
                        --let mbuffs         = listToMaybe buffers
                        let screenLogger  str = G.postGUIAsync $ updateBufferAtEnd (buffers !! 0) str
                        let processLogger str = G.postGUIAsync $ updateBufferAtEnd (buffers !! 1) str -- ) mbuffs
                        mydia <- myDialogNew -- remove fromData Below
                        let writers = [wrt, screenLogger,processLogger]
                        let compTMML      = bs2HMDIF2bs_aux Nothing path writers Nothing restr (defect,Max) yDist
                        let discp         = "Running mml2ds on "++ defect
                        G.set (myDia mydia) [G.windowTitle G.:= discp ]
                        processWithScroll (Just mydia) (Just discp) labl wrt (myTry $ compTMML (Just "CR1"))
                    else do
                        let str = "HMDIF file does not exist. Please create a HMDIF file,\
                                  \ using the 'update source file' option, and try again. "
                        runMsgDialog  Nothing str  Error
--
