module ControlFlow (
   -- optionsWindow
   ---,visualize
    makeAnalViewInterface
   ,convertSimFileWindow
   ,getRestriction
    , ResultType  ( CSV, SScanner, Accident, SScrim)
    ,ioLogger
--    , WindowTypes (NetworkAnalysis, SchemAnalysis, AccidentAnalysis
--                   , SchemeBuilder, AnalyseOrExtract, DNAdisplay)

    ) where

import qualified Graphics.UI.Gtk as G
import qualified Data.ByteString.Lazy.Char8 as L
--import Data.List (find,foldl',nub)
import Data.List (sort,find,delete,foldl', delete,findIndex, nub)
import Data.Maybe(isJust, fromJust,listToMaybe)
import System.Random (newStdGen)
import Data.Char (isAlpha,isDigit,isSpace,isAlphaNum)
import Control.Monad (liftM,liftM2)
import Control.Concurrent (forkIO)
import Data.IORef
--import Diaplay (DataType (..))
import System.Directory (doesFileExist, doesDirectoryExist,createDirectory,removeFile,
                         getPermissions,setPermissions,writable, readable,getDirectoryContents,createDirectoryIfMissing)


-- my definitions
import DrawingFrame (fileBrowser,mkDnaDisplayFrame,isValid)
--import InterfaceQAWindows (mkResultBox)
import FileIO (getAlreadyExtracted)
import RprGD2 (mml2DsGD)
import LMisc (mkIntervals,tokens)
import Parallel (forkProcess,forkProcessWithScroll,processWithScroll,myTry)
--import Parallel (forkProcess,forkProcessWithScroll,processWithScro,ll,myTry,forkProcessWithID)
import ProcessFile (Alignment (..),display,SchemeRec (..), SchemeInRec (..)
                    ,readSchemeRec1,schemeRec2String)
import HMDIFPrelude (conditions, typesAndDetails,string2SCODE ,Defect (..),
                     bs2Int',scode2align,comma,splitAtChunkBS,bsReadChr) --  , typesAndDetails,scode2align,(<>))
import ReadFromDataFiles2 (bs2HMDIF2bs_aux)
import RprGD2 (OptimisationType(..))
import FileIO (checkIfExtracted,removeExtracted,csv2HMDIF_aux)
import RprMix2 (OModel) -- ,printElm,Mmodel (MkModel),r_n)
import NiceFork
import GlobalParameters
{--

--
import ControlFlow (makeAnalViewInterface, convertSimFileWindow, ResultType (..)) --- ,W

data LoadFile = HMDIF
                | SchemeFile  -- HMDIF files
                | CommaSeparated Int  -- comma separated file, indicating the number of commas
                                      -- in the file
--}
data ResultType  = CSV | SScanner | Accident | SScrim -- NScanner | SScanner | Accident | NScrim | SScrim
--- files used in the analysys
--getHmDifpath = do

--hmDifPath = "./SimulationFiles/hmdifPath.txt" --- "./FilePaths/pathsFile.txt"
-- mPath <- liftM (listToMaybe . L.lines ) $ L.readFile
mixfile = "./mmlTdsFiles/mixture.txt"
cutsfile = "./mmlTdsFiles/onlycuts.txt"
seedfile = "./mmlTdsFiles/seed.txt"
roadClasses = "./roadAttributes/roadClasses.txt"
--
-- paths
defectsDir = "./Defects"
hmdifPath = "./SimulationFiles/hmdifPath.txt"
{-
scannerPath = "./SimulationFiles/scannerPath.txt"
scrimPath = "./SimulationFiles/scrimPath.txt"
csvPath = "./SimulationFiles/csvPath.txt"
--
resultsFile = "./SectionResults/results.txt"
--}
roadNamesPath = "./SimulationFiles/roadNamesPath.txt"
--mixfile = "./mmlTdsFiles/mixture.txt"
--
data Scheme = Scheme {
    --comboBox :: G.HBox,
    condSCmb :: G.ComboBox,
    rClassCmb :: G.ComboBox,
    rNameCmb :: G.ComboBox,
    sCodeCmb :: G.ComboBox,
    rCodeCmb :: G.ComboBox,
    sStart    :: G.Entry,
    sEnd      :: G.Entry
    }

--- get the restriction
 -- get the negative slope option
getRestriction :: IO (Maybe Bool)
getRestriction = do
  let setRestr bs = if bs == L.pack "2" then Nothing else (Just (bs == L.pack "0"))
  (liftM (head . head) $ bsReadChr ',' mixfile) >>= return . setRestr
----
getNewScheme :: IO Scheme
getNewScheme = do
    combos <- sequence $ replicate 5 G.comboBoxNewText
    entries <- sequence $ replicate 2 G.entryNew
    let [a,b,c,d,e] = combos
    mapM_ (\(a,n) -> G.widgetSetSizeRequest a n (-1)) $ zip combos [260,100,130,100,100,100]
    mapM_ (\ent -> do
                G.entrySetWidthChars ent 6
                G.entrySetMaxLength ent 6) entries
    let [e1,e2] = entries
    return $ Scheme a b c d e e1 e2

--- get the combos from a scheme
schemeCombos :: Scheme -> [G.ComboBox]
schemeCombos schems = [condSCmb schems,
                       rClassCmb  schems,
                       rNameCmb  schems,
                       sCodeCmb  schems,
                       rCodeCmb  schems]

--schemeEnries
schemeEntries :: Scheme -> [G.Entry]
schemeEntries scm = [sStart scm, sEnd scm]

--
scheme2Strings :: Scheme -> IO [String]
scheme2Strings scm = do
    let mStr = maybe "" id
    comboStrings <-  mapM ((liftM mStr). G.comboBoxGetActiveText)  $ schemeCombos scm
    liftM (comboStrings ++) . mapM G.entryGetText $  schemeEntries scm


--
ioLogger :: IO (IORef (String -> IO()))
ioLogger = newIORef (\_ -> return ())


--
data Analysis = Analysis {
    condsCmb :: G.ComboBox,
    cwCmb :: G.ComboBox,
    mfYrs    :: G.Entry,
    trLen    :: G.Entry
    }

--
anal2Strings :: Analysis -> IO [String]
anal2Strings anl = do
    let mStr = maybe "" id
    comStr <- liftM mStr . G.comboBoxGetActiveText  $ condsCmb anl
    liftM (comStr : ) . mapM G.entryGetText $   [mfYrs anl, trLen anl]

--
getNewAnalysis :: IO Analysis
getNewAnalysis = do
    [combo, cr] <- sequence $ replicate 2 G.comboBoxNewText
    G.widgetSetSizeRequest combo 160 (-1)
    entries <- sequence $ replicate 2 G.entryNew
    mapM_ (\ent -> do
                G.entrySetWidthChars ent 6
                G.entrySetMaxLength ent 6) entries
    return $ Analysis combo  cr (entries !! 0) (entries !! 1)
--------
--- the options for analysis
--data CondType = SCANNER | SCRIM  deriving Show
data AnalType = NetworkAnalysis |  SchemeAnalysis | SectionAnalysis deriving (Eq, Show)
data ForAnalysis = ForAnalysis {
    atype   :: AnalType,
    fDefect :: String,
    fSgSrg  :: Maybe (String,String),
    fCway   :: Maybe String, -- [String],
    yrsDst  :: (Double,Double),
    nSlope  :: Maybe Bool,
    optType :: OptimisationType -- Rapid , FromData
    } deriving (Eq,Show)
-- fDefect,fCway

data Three a b c = Lft a | Mid b | Rgt c
---------------------
-- applying the analysis function ot a  list of selections
applyAnalysis :: (String -> IO ()) -> (String -> IO ()) -> FilePath -> [ForAnalysis] -> IO ()
applyAnalysis screenLogger write path =  mapM_ fanl2AnlFun
    --createDirectoryIfMissing False defectsDir
    --
    where
        ---
        --mkInc a = if a then (L.pack "1") else  (L.pack "0")
        ---
        getAlnAndDefect :: String -> Maybe (String, Alignment)
        getAlnAndDefect str =
            case (string2SCODE str) of
                Nothing ->  Nothing
                Just scd -> Just (str, if scd `elem` crackingParms then Avg else Max)
            where
                crackingParms = [LTRC,LWCL,LWCR,LECR,LRCR,LMAP,LSUR,LOVD]
        ---
        fanl2AnlFun :: ForAnalysis -> IO ()
        fanl2AnlFun fa =
            case (getAlnAndDefect $ fDefect fa) of
                Nothing -> return ()
                Just faa -> do
                    --L.writeFile mixfile (mkInc $ nSlope fa) -- the the negative slope flag
                    bs2HMDIF2bs_aux (fSgSrg fa)
                            path
                            [write, screenLogger, \_ -> return ()]   -- a printing function
                            Nothing -- we do not analyze schemes
                            (nSlope fa)
                            faa
                            (yrsDst fa)
                            (fCway fa)


{--
Maybe (String, String) -> --- section and sorrogate
        FilePath ->  -- the filepath with the HMDif file
        (String -> IO ()) -> -- function to write to a progress dialog
        Maybe SchemeRec -> -- determine whether of not u are analysing schemes
        (String,Alignment) ->
        (Double, Double) ->  -- years and distance
        Maybe String ->  --- if should use both carraige ways
--}
-----------------------------------------------------------------------
{-- file Paths
hmdifPath  = "./FilePaths/hmdif.txt"
schemePath = "./FilePaths/schemeFile.txt"
pathsFile = "./FilePaths/pathsFile.txt"
toAnalFile = "./defectYearsDistance.txt"
--}
setFrameTitle :: G.Frame -> String -> IO ()
setFrameTitle frame str = G.set frame [G.frameLabel G.:= str ]
----------------------------------------------------------------------------------------------------
--- User Interface for analysis
----------------------------------------------------------------------------------------------------
--mkAnalysisDia --- continue from here ---
mkAnalysisDia :: [G.TextBuffer] -> Bool -> Bool -> [String] -> IO MyDialog
mkAnalysisDia buffs analysis isScanner forCmbo = do
     mdia <- myDialogNew
     modifyIORef (title mdia) (++ "Analysis Options")
     anlRef <- newIORef []
     myDialogAddWidget mdia =<< mkAnalysysBox anlRef forCmbo
     buttons <-  mapM G.buttonNewWithMnemonic ["_Analyse","_Cancel"]
     mapM (myDialogAddButton mdia) buttons
     --- handles for buttons
     G.on (buttons !! 1) G.buttonActivated $ G.widgetDestroy (myDia mdia)
     ---
     G.on (buttons !! 0) G.buttonActivated $ do
        -- applyAnalysis ::  FilePath -> [ForAnalysis] -> IO ()
        mPath <- liftM (listToMaybe . L.lines ) $ L.readFile hmdifPath
        case mPath of
            Nothing -> runMsgDialog  (Just "Path Retrieve Error")  "Error retrieving HMDIF source path " Error
            Just bs -> do
                let remJust xs = [a | ys <- xs , isJust ys, let (Just a) = ys]
                anlList <- liftM remJust $ readIORef anlRef
                if null anlList then
                    runMsgDialog  (Just "Nothing to Analyse") "No options set for analysis"  Error
                else do
                    let (deft, pth)   = splitAtChunkBS (L.pack ",") bs -- deft
                    let path          = L.unpack pth
                    let def           =  L.unpack deft
                    -- print ("path is: "++ path)
                    pathExist   <- doesFileExist path
                    if pathExist then do
                        -- "SCANNER HMDIF" ,  "SCRIM HMDIF"
                        let aType = if isScanner then "SCANNER HMDIF" else "SCRIM HMDIF"
                        if def /= (filter isAlpha aType) then do
                            let str = "The current HMDIF filein not valid for" ++ aType ++ " files.\
                                       \ Please update the a valid HMDIF file using the \
                                       \ 'update source file' option, and try again. "
                            runMsgDialog  (Just "Invalid HMDIF File  ") str  Error
                        else do
                            G.widgetDestroy $ myDia mdia
                            labl <- G.labelNew Nothing
                            let wrt string =  G.postGUIAsync $ G.labelSetText labl string
                            let screenLogger = maybe (\_ -> return ()) (\bf str -> G.postGUIAsync $ updateBufferAtEnd bf str) (listToMaybe buffs)
                            let discp         = "Running mml2ds on selected conditions" -- ++ defect
                            proGdia <- myDialogNew
                            G.set (myDia proGdia) [G.windowTitle G.:= discp]
                            processWithScroll (Just proGdia) (Just discp) labl wrt $ applyAnalysis screenLogger wrt path anlList
                    else do
                        let str = "HMDIF file does not exist. Please point to a HMDIF file,\
                             \ using the 'update source file' option, and try again. "
                        runMsgDialog  (Just "File Read Error") str  Error


        --G.on nextButt G.buttonActivated $ do
     --}
     return mdia
     where
        -- get Analysis options from selections
        getAnalisiOptions :: String -> -- Bool -> -- Maybe (String, String) ->
                             Maybe String -> (Int,Int) -> IO (Maybe ForAnalysis)
        getAnalisiOptions selectedString  cWays (y,d) = do
            nSlp <- getRestriction
            if anlTyp == "Network" then
                 return . Just $ ForAnalysis NetworkAnalysis deft Nothing cWays (yy,dd) nSlp optY
            else if anlTyp == "Scheme"  then
                 return . Just $ ForAnalysis SchemeAnalysis deft (mSSorg 3) cWays (yy,dd) nSlp optY
            else if anlTyp == "Section" then
                 return . Just $ ForAnalysis SectionAnalysis deft  mssN cWays (yy,dd) nSlp optY
            else return  Nothing
            where
                optY           =  FromData
                (anlTyp, rest) = break (== ':') (filter (not . isSpace) selectedString)
                rss            = tokens (dropWhile (not . isAlphaNum) rest) ','
                deft           = takeWhile isAlphaNum $ rss !! 0
                (yy,dd)        = (fromIntegral y, fromIntegral d)
                splitAtChe     = break (== '-')
                mssN           = Just (rss !! 2, "")
                ---
                mSSorg  n      = Just (a,tail b) -- ) (
                    where
                         (a,b) = break (== '-') (rss !! n)

        ---
        mkAnalysysBox :: IORef [Maybe ForAnalysis] -> [String] -> IO G.VBox
        mkAnalysysBox nextRef choices = do
            anl <- getNewAnalysis
            --negSlopesCB <- G.checkButtonNewWithLabel "Include Negative Slopes"
            --- <-  G.frameNew
            [mainBox,buttonRow]   <- sequence . replicate 2 $ G.hBoxNew False 0
            G.widgetSetSizeRequest mainBox 700  (-1)
            optionsBox <- G.vBoxNew False 1
            [entryFrame, optionsFrame] <- sequence $ replicate 2 G.frameNew
            G.containerAdd optionsFrame optionsBox
            (swn, tBuff) <- makeScrolledEntryArea BScroll False True
            G.containerAdd entryFrame swn
            seps  <- sequence $ replicate 2 G.hSeparatorNew
            ---
            let strList = ["Average treatment length for selected condition ",
                           "Average years between maintenance interventions "] -- Enter number to limit entry file: "]
            let labelEnt a b = labelLeftofWidgetE' a (6 * length a) b  True
            let clearEntry entry = G.entrySetText entry ""
            let entries = [mfYrs  anl, trLen anl]
            let combo  = condsCmb anl
            --let options = ["Enter options individually ", "Enter options for all selections "]
            --let optimisations = ["Optimise from data: ", "Rapid selection: "]
            let mkHTable tf homo sp w = tableWithWidgetList tf  w homo sp
            --
            mapM_ (G.comboBoxAppendText combo) choices
            mapM_ (G.comboBoxAppendText $ cwCmb anl) ["CR1","CL1"]
            ydCol <- (mkHTable True False 6) =<< mapM (uncurry labelEnt) (zip strList entries)
            --- buttons
            let buttonNames = ["Advanced Options", "  Next  "]
            [adv, nextButt]  <- mapM G.buttonNewWithLabel  buttonNames
            --mapM_ (\b -> G.widgetSetSizeRequest b 30  (-1)) [adv, nextButt]
            mapM_ (\w -> G.boxPackEnd buttonRow w G.PackNatural 3) [nextButt] --, adv]
            (analBothCW,nsBox) <- checkBoxWithLeftLabel "Analyse both carriage ways"
            ----
            ckButtons   <- makeRadioButtonGroupFromStrings  ["","Restrain to positive  ",
                                                     "Restrain to negative",
                                                     "Unrestrained "]
            --
            let setRst (b,n) = G.on b G.toggled $ do
                            isTogg <- G.toggleButtonGetActive b
                            if isTogg then L.writeFile mixfile (L.pack $ show n) else return ()
            mapM_ setRst $ zip (tail ckButtons) [0,1 .. ]
            ----}
            selectWayLabel <- G.labelNew (Just "Select carriage way: ")
            ----
            (optsRow,_) <- horizontalWidgetList  True (tail ckButtons) True
            let setCWoption = G.widgetSetSensitive (cwCmb anl) =<< (liftM not $ G.toggleButtonGetActive analBothCW )
            setCWoption >> G.on analBothCW G.toggled setCWoption
            --G.boxPackEnd nsBox chkButton G.PackNatural 3
            --G.boxPackEnd nsBox (chkButtons !! 1) G.PackNatural 3
            G.boxPackEnd nsBox (cwCmb anl) G.PackNatural 3
            G.boxPackEnd nsBox selectWayLabel G.PackNatural 3
            -- packing into the entrie box --- condsCmb
            G.boxPackStart optionsBox combo G.PackNatural 3
            --G.boxPackStart optionsBox radiosRow G.PackNatural 3
            G.boxPackStart optionsBox (seps !! 0) G.PackNatural 0
            G.boxPackStart optionsBox ydCol G.PackNatural 3
            G.boxPackStart optionsBox nsBox G.PackNatural 3 -- optRadiosRow
            G.boxPackStart optionsBox optsRow G.PackNatural 3
            --
            G.boxPackStart optionsBox (seps !! 1) G.PackNatural 0
            G.boxPackStart optionsBox buttonRow G.PackNatural 3
            --- into the main packing box
            G.boxPackStart mainBox optionsFrame G.PackNatural 4
            G.boxPackStart mainBox entryFrame G.PackGrow 4
            --
            let updateButt b io = G.on b G.toggled io --
            -- handle for the combos, etc --
            optionRef <- newIORef []
            ---
            G.on nextButt G.buttonActivated $ do
                restr <- liftM (maybe [] (\_ -> "1") . listToMaybe . filter (== True)) $ mapM G.toggleButtonGetActive (tail ckButtons)
                options <-  anal2Strings anl
                let mStr = Just . maybe [] (: [])
                let messages = ["select from chosen conditions for analysis",
                                "enter the average treatment length for selection ",
                                "enter the years between maintenance interventions. ",
                                "select an option for  restraining slopes"]
                let missing = [i | (s,i) <- zip (options ++ [restr]) [0..], null s]
                let getMissing xs = [messages !! i | i <- xs]
                --- validate entries
                let getVWay b = if b then (return Nothing) else liftM mStr . G.comboBoxGetActiveText $ cwCmb anl
                ---(getVWay =<< G.toggleButtonGetActive chkButton )
                let invMsg  = [" average treatment length "," years between maintenance interventions " ]
                let getValid xs = [invMsg !! i | i <-  xs]
                let invalid = [i | (s,i) <- zip (tail options) [0 ..], (entVal s) < 0]
                ----
                if null missing then do
                    -- validate the entries
                    cway <- getVWay =<< G.toggleButtonGetActive analBothCW
                    case cway of
                        Just [] ->
                            runMsgDialog (Just "No carriage way") "Please select an option for carriage way"  Error
                        anyElse ->
                            if (null invalid) then do
                                 -- check the carriage way
                                let toAdd = (options !! 0)
                                let addedMsg = "You have already added "++ toAdd
                                let [y,d] = map entVal $ tail options
                                mAdd <- getAnalisiOptions toAdd (liftM head cway)  (y,d)
                                added <- liftM (filter (== mAdd)) $ readIORef nextRef
                                if length added > 0 then
                                    runMsgDialog (Just "Already Added") addedMsg  Info
                                else do
                                    modifyIORef nextRef (mAdd :)
                                    updateBufferAtEnd tBuff toAdd
                                    mapM_ clearEntry entries
                                    G.comboBoxRemoveText combo 0
                                    --clearAdd optionRef combo (filter (/= toAdd) choices)
                                    --print mAdd
                            else do
                                let msg = "The value entered for" ++ (setErrMsg $ getValid invalid) ++ "is not valid."
                                runMsgDialog  (Just "Invalid Entries") msg  Error
                else do
                    let msg = " You did not " ++ (setErrMsg $ getMissing missing)
                    runMsgDialog  (Just "Missing Options") msg Error
                    -- sel <-  G.comboBoxGetActiveText rclass
                    --
            --labelOverWidget :: G.WidgetClass widget => String -> widget -> Maybe Int -> IO G.VBox
            labelOverWidget "\t Select options for analysis\t " mainBox (Just 400)



---------------------------------------------------------------------------------------------------
-- User interface with options for analysis and viewing interface
---------------------------------------------------------------------------------------------------
makeAnalViewInterface :: [G.TextBuffer] -> Bool -> IO (Maybe MyDialog)
makeAnalViewInterface buffs anal = do
    let pathsFile = "./FilePaths/pathsFile.txt"
    if anal then do
        exists <- doesFileExist pathsFile
        if exists then
            (return . Just) =<< makeAnalViewInterface_aux buffs anal
        else do
             return Nothing
    else
        (return . Just) =<< makeAnalViewInterface_aux buffs anal -- =<< makeAnalViewInterface_aux analysis

-- originally intended for this view to be the same for analysis and visualisation, but
-- changed my mind. Currently only used for analysis
makeAnalViewInterface_aux :: [G.TextBuffer] -> Bool -> IO MyDialog
makeAnalViewInterface_aux buffs analysis = do
    let analType = if analysis then "analysize" else "visualize"
    let windowMsg = "Select data to "++ analType
    ----------------------------------------------------------------
    mdia <- myDialogNew
    G.widgetSetSizeRequest (myDia mdia) 760 (-1)
    -- mainframe <- G.frameNew
    -- windowResizable
    G.set (myDia mdia) [G.windowResizable G.:= True]
    selectedRef <- newIORef []
    modifyIORef (title mdia) (++ windowMsg)
    ----------------------------------------------------------------
    ---- the scanner or scrim radio butttons
    conditionTypes_1  <- makeRadioButtonGroupFromStrings ["", "SCANNER","SCRIM"]
    let conditionTypes = tail conditionTypes_1
    -- [d1,d2] <- mapM (\st -> G.radioButtonNewWithLabel st) ["", ""]mapM (G.radioButtonNewWithLabelFromWidget d2 )
    let (conditionsLabels, options)  = unzip typesAndDetails
    condTypes1 <- makeRadioButtonGroupFromStrings ("" : conditionsLabels) -- mapM (G.radioButtonNewWithLabelFromWidget d1 )
    let condTypes = tail condTypes1
    analTypes <- makeRadioButtonGroupFromStrings ["Network","Section"] -- "Scheme/Road",
    --
    G.widgetSetSensitive (analTypes !! 1) analysis
    --G.widgetSetSensitive (analTypes !! 1) (not analysis)
    G.widgetSetSensitive (analTypes !! 0) analysis
    -- getConditions mbutt Nothing SchemAnalysis condTypes
    let (rBUuttons, optTypes) = (conditionTypes ++ condTypes ++ analTypes , map (map display) options)
    let wrtr str = G.postGUIAsync $ updateBufferAtEnd (buffs !! 0) str
    (optionsBox, endLabel)  <- analysisTop wrtr Nothing  selectedRef rBUuttons optTypes
    -- make optionBox wider than the default
    --G.widgetSetSizeRequest optionsBox 850  (-1)
    myDialogAddWidget mdia optionsBox
    buttons <-  mapM G.buttonNewWithMnemonic ["_Continue","_Cancel"]
    mapM (myDialogAddButton mdia) buttons
    -- add the label at the bottom of the dialog
    G.boxPackEnd (lower mdia) endLabel G.PackGrow 4
    -- handles for buttons
    G.on (buttons !! 1) G.buttonActivated $ G.widgetDestroy (myDia mdia)
    G.on (buttons !! 0) G.buttonActivated $ do
        let flattn = concat . intercalate ","
        selected <- liftM (map (\(a,b) -> a ++ flattn b)) $ readIORef selectedRef
        if null selected then do
            let nullMsg = "You have not entered any condition to analyse. Please select a \
                        \ condition or click on 'Cancel' to exit this option"
            runMsgDialog  (Just "Nothing selected") nullMsg  Error
        else do
            G.widgetDestroy (myDia mdia)
            isScanner <-  G.toggleButtonGetActive (conditionTypes !! 0)
            mkAnalysisDia buffs analysis isScanner selected >>= myDialogShow
    --
    return mdia


----------------------------------------------------------------------------------------------------
--- creating my analysis interface -- IORef [(ResultType,String, [String])]
----------------------------------------------------------------------------------------------------
analysisTop :: (String -> IO()) -> Maybe String -> IORef [(String, [String])] ->
               [G.RadioButton] -> [[String]] -> IO (G.HBox, G.Label)
analysisTop logger mScanner selectedRef rButtons options =  do
    topBox <-  G.vBoxNew False 0
    -- message label
    msgLabel <-  G.labelNew Nothing
    --
    [topLeftBox, leftBox] <- sequence . replicate 2  $ G.vBoxNew False 0
    mainBox  <-  G.hBoxNew False 0
    [condFrame, topFrame, bottomFrame, bottomLeftFrame] <- sequence $ replicate 4 G.frameNew
    ---------------------------------------------------------------------------------------------
    -----
    scm <- getNewScheme
    -- let scannerConds = head $ schemeCombos scm
    let scombos = tail $ schemeCombos scm
    let [scannerConds,rclass, rname, scode,rcode ] = schemeCombos scm
    --
    [conditionComboAddRef,rNameRef, rCodeRef, sCodeRef] <- sequence . replicate 4 $ newIORef []
    -- ioRef or the condition type
    analTypeRef <- newIORef $ Right $ Left scannerConds -- (Either  (scannerConds,rclass,scode))
    scanrScrimRef <- newIORef True
    -- contineue here
    let (scnnr, rest) = splitAt 7 (drop 2 rButtons)
    -- update the main combo
    let updateCombo (rButt, i) = G.on rButt G.toggled $ clearAdd conditionComboAddRef scannerConds (options !! i)
    let clearEntry entry = G.entrySetText entry ""
    mapM_ updateCombo (zip scnnr [0,1 ..])
    --- update the road class
    list <- readSchemeRec1 (\_ -> return ())

    ----
    mapM_ (G.comboBoxAppendText rclass) =<< liftM sort getRClasses
    ---
    G.on rclass G.changed $  do
        cls <- liftM (maybe "" id) $ G.comboBoxGetActiveText rclass
        let validClasses = filter ( (== cls) . scSrg) list
        clearAdd rCodeRef rcode ((nub .  map clsCode)  validClasses)
    G.on rcode G.changed $  do
        cls <- liftM (maybe "" id) $ G.comboBoxGetActiveText rcode -- liftM (maybe "" (takeWhile isAlpha)) $
        let validCodes = filter ((== cls) . clsCode) list -- classsCode
        --let writer = G.labelSetText msgLabel
        let string = "Updating entries for road code: "++ cls
        let updateEntries = do  clearAdd rNameRef rname (nub $ map rdName  validCodes)
                                clearAdd sCodeRef scode (map  scnrCode validCodes)
                                G.labelSetText msgLabel ("Finished " ++ string)
        G.labelSetText msgLabel (string ++ " ...")
        updateEntries
    --------------------------------------------------------------------------------------------------
    --- clearAdd cRef combo list
    -- condFrame
    let frameLabels = ["Condition Type", "Select Conditions","Analysis Type"]
    mapM_ (uncurry G.frameSetLabel) $ zip [condFrame,topFrame, bottomFrame] frameLabels
    G.containerAdd topFrame topBox
    ---------------------------------------------------------------------------------------
    -- activate top frame for when scanner is selected
    -- (rButtons !! 0)
    let updateScannrScrim b1 butt = do G.on butt G.toggled $ do
                                        isScanner <- G.toggleButtonGetActive  b1
                                        G.widgetSetSensitive topFrame isScanner
                                        modifyIORef scanrScrimRef (\_ -> isScanner)
                                        if butt == b1 then
                                            -- need to check if the scanner path fexists and if it does, set the path file to the that
                                            checkAndUpdatePath "SCANNER"
                                        else do
                                            checkAndUpdatePath "SCRIM"
    mapM_ (updateScannrScrim (rButtons !! 0)) (take 2 rButtons)
    --G.containerAdd condFrame =<< tableWithWidgetList False (take 2 rButtons) True 3
    --
    [a1,a,b,c,d] <- sequence [ts | (butts ,_) <- mkIntervals rButtons [2,3,3,1,2]
                            , let ts = tableWithWidgetList False  butts True 5]
    mapM (uncurry G.containerAdd) [(condFrame, a1),(bottomFrame, d)]
    --
    let str  = maybe "SCANNER Conditions: " ((++ " Conditions: ") . id) mScanner
    comboRow <- labelLeftofWidgetE str 120 scannerConds False
    -- packing the topBox
    mapM_ (\(w,n) -> G.boxPackStart topBox w G.PackNatural n) [(a,0),(b,4)]
    G.boxPackStart topBox comboRow G.PackNatural 1
    G.boxPackStart topBox c G.PackNatural 1
    -- packing the mainBox ----------------------------------------------------------
    mapM (\w -> G.boxPackStart topLeftBox w G.PackNatural 6) [condFrame, topFrame, bottomFrame]
    --G.boxPackEnd mainBox leftPanes G.PackGrow 3
    (leftPanes, entries) <- displayPane True
    buttons <- mapM G.buttonNewWithLabel [" >>> "," Clear "]
    (box, lframe,lvbox) <- schemeAndSection scm buttons
    -- desenditive some stuff
    G.widgetSetSensitive lvbox False
    G.widgetSetSensitive topFrame False
    --
    forkIO $ showAlreadyAnal (entries !! 0)
    --
    G.containerAdd bottomLeftFrame box
    -- packing the main box
    G.boxPackStart leftBox topLeftBox G.PackGrow 3 -- bottomLeftFrame
    G.boxPackStart leftBox bottomLeftFrame G.PackGrow 3
    ---
    G.boxPackEnd mainBox leftPanes G.PackGrow 3
    G.boxPackStart mainBox leftBox G.PackGrow 3
    ------------------------------------------------
    updateConditionType rest scm lvbox lframe analTypeRef
    -- handle for adding buttons
    G.on (buttons !! 0) G.buttonActivated $ do
        errOrOK <- verfyOptions scm scanrScrimRef analTypeRef
        case errOrOK of
            Right errorMessage -> do
                let error = "You have not entered: " ++ errorMessage
                runMsgDialog  (Just "Input Error") error  Error
            Left xs    -> do
                let (hs, rs) = (head xs, tail xs)
                --modifyIORef selectedRef ((hs,rs) :)
                --let emptyMessage = "There is nothing to add"
                let selectedMsg = "You have already selected "++ hs ++ " " ++ show rs
                selected <- liftM (filter (== (hs, rs))) $ readIORef selectedRef
                if length selected > 0 then
                    runMsgDialog  (Just "Already Selected") selectedMsg  Info
                else do
                   -- Nothing -> runMsgDialog  (Just "Empty Options") emptyMessage  Info
                    let flattn = concat . intercalate ","
                    updateBufferAtEnd (entries !! 1) (hs ++ flattn rs)
                    mapM_ clearEntry  $ schemeEntries scm
                    modifyIORef selectedRef ((hs,rs) :)
                --return ()
    --- option for clearing the
    G.on (buttons !! 1) G.buttonActivated $ do
       mapM_ clearEntry  $ schemeEntries scm -- clear the entries
       modifyIORef selectedRef (\_ -> []) -- empty the ioref
       clearTextBuffer (entries !! 1) -- clear the buffer
    ------------------------------------------------
    return (mainBox,msgLabel)
    where -- scheme2Strings :: Scheme -> IO [String]
        verfyOptions schm sScrimRef analRef = do  -- returns Either (Three [String] [String] [String]) String
            isScanner <- readIORef sScrimRef
            let typeName = if isScanner then "SCANNER" else "SCRIM"
            let strings = [typeName ++ " condition", "Road class","Road Name",typeName ++" section code",
                           "Road code","Start of subsection","End of subsection"]
            let getMissing xs = [strings !! i | i <- xs]
            input <- scheme2Strings schm
            --
            let missing = [i | (s,i) <- zip input [0..], null s]
             --  Three a b c = Lft a | Mid b | Rgt c
            aref <- readIORef analRef
            case aref of
                Left _ -> do
                    if null missing  then
                        return $ Left  (("Scheme: "):input)
                    else if (not isScanner)  then
                        return $ Left ("Scheme: " : "SFC - SCRIM Parameter" : (tail input))
                    else
                        return $ Right . setErrMsg $ getMissing missing
                Right (Left cm) -> do
                    let miss = filter (\i -> i == 0 ) missing
                    if (null miss) then
                        return $ Left ["Network: ",input !! 0]
                    else if (not isScanner)  then
                        return $ Left ["Network: " , "SFC - SCRIM Parameter"]
                    else
                        return $ Right . setErrMsg $ getMissing miss
                Right (Right _ ) -> do
                    let miss = filter (\i -> i == 0 || i == 1 || i == 3) missing
                    let smiss = filter (\i -> i == 1 || i == 3) missing
                    if (null miss)  then
                        return $ Left ["Section: ",input !! 0, input !! 1, input !! 3]
                    else if (null smiss && not isScanner)  then
                        return $ Left ["Section: ","SFC - SCRIM Parameter", input !! 1, input !! 3]
                    else
                        if isScanner then
                            return $ Right . setErrMsg $ getMissing miss
                        else
                            return $ Right . setErrMsg $ getMissing smiss
        ---
        updateConditionType butts scm lvbx frame2 analRef = do
            let [b1,b3] = butts
            let [network,section] = butts
            let combos = schemeCombos scm
            let entries = schemeEntries scm
            G.on b1 G.toggled $ do
                active <- G.toggleButtonGetActive  b1--print "section"
                if active then do
                    rnamesExist <- doesFileExist roadNamesPath
                    if rnamesExist then
                        return ()
                    else do
                        let message = "There is no file from which to extract the roadnames and \
                                       \conditions for the filters. Would you like to update the roadNames path? "
                        runDecisionMessageWindow (Just "No Roadnames") message $ convertSimFileWindow logger
                        --runMsgDialog  (Just "No Roadnames") nullMsg  Error
                    --- continue regardless
                    G.widgetSetSensitive lvbx False
                    modifyIORef analRef (\_ -> Right . Left $ head combos )
                else return ()
            {-
            G.on b2 G.toggled $ do
                active <- G.toggleButtonGetActive  b2--print "section"
                if active then do
                    G.widgetSetSensitive lvbx True
                    G.widgetSetSensitive frame2 True
                    mapM_ (flip G.widgetSetSensitive True) combos
                    mapM_ (flip G.widgetSetSensitive True) entries
                    modifyIORef analRef (\_ -> Left scm)
                else return ()
            --}
            G.on section G.toggled $ do
                -- check the roadnames path and give a message if any path was loaded
                active <- G.toggleButtonGetActive  section--print "section"
                if active then do
                    G.widgetSetSensitive lvbx True
                    let hs = [ (c,i/= 2) | (c,i) <- zip (tail combos) [1  .. ]]
                    --print $ map snd hs
                    G.widgetSetSensitive lvbx True
                    --mapM_ (uncurry G.widgetSetSensitive ) hs
                    mapM_ (flip G.widgetSetSensitive True) combos
                    G.widgetSetSensitive frame2 False
                    modifyIORef analRef (\_ -> Right $ Right (combos !! 0, combos !! 1, combos !! 3) )
                else return ()
        --
        displayPane :: Bool -> IO (G.VPaned , [G.TextBuffer])
        displayPane topFrameSensitive = do
            (sws, entries) <-  liftM unzip . sequence . replicate 2 $ makeScrolledEntryArea BScroll False True
            -- (bsw, bottomEntry) <-  makeScrolledEntryArea BScroll False True
            frames <- sequence $ replicate 2 G.frameNew
            mapM_ (uncurry G.containerAdd) $ zip frames sws
            let frameLabels = ["Conditions already analysed " ,"Conditions selected for analysis"]
            mapM_ (uncurry G.frameSetLabel) $ zip frames frameLabels
            G.set (frames !! 0) [G.widgetSensitive G.:= topFrameSensitive ]
            vpane <- vPanedArea (frames !! 0) (frames !! 1)  100
            return (vpane, entries)
        --
        schemeAndSection :: Scheme -> [G.Button] -> IO (G.HBox, G.Frame, G.VBox)
        schemeAndSection scm buttons = do
            let scombos = tail $ schemeCombos scm
            let [rclass, rname, scode,rcode ] = scombos
            let entries = schemeEntries scm
            let entryLabels = ["Start of subsection:", "End of subsection:"]
            let comboLabels = ["Road class:", "Road name: ", "Section code:", "Road code:"]
            --
            comboRows <- mapM (\(s,w) -> labelLeftofWidgetE s 83 w False) (zip comboLabels scombos)
            entryRows <- mapM (\(s,w) -> labelLeftofWidgetE' s 100 w False) (zip entryLabels entries)
            -- the main HBox
            [mainBox,frameBox,optionsRow ] <-  sequence . replicate 3 $ G.hBoxNew False 0
            [mVBox, leftVBox, rightVBox] <- sequence . replicate 3  $ G.vBoxNew False 0
            optionFrames <-  sequence $ replicate 2 G.frameNew
            --
            mapM_ (uncurry G.containerAdd) (zip optionFrames [leftVBox, rightVBox])
            --optionsRow <- tableWithWidgetList False optionFrames False 36
            G.boxPackStart optionsRow (optionFrames !! 0) G.PackGrow 3
            G.boxPackStart optionsRow (optionFrames !! 1) G.PackNatural 3
            --
            buttonList <- tableWithWidgetList True  buttons True 16
            -- packing the intermediate boxes
            mapM (\w -> G.boxPackStart rightVBox w G.PackNatural 5) entryRows
            mapM (\w -> G.boxPackStart leftVBox w G.PackGrow 2) [xs | (xs,i) <- zip comboRows [1 .. ],  i /= 2 ]
            ----------------------------------------------------------------------------------------------------
            -- packing the mainVBox
            G.boxPackStart mVBox (comboRows !! 1) G.PackNatural 3
            G.boxPackStart mVBox optionsRow  G.PackNatural 3
            -- packing into the main box
            G.boxPackEnd mainBox buttonList G.PackNatural 3
            G.boxPackEnd mainBox mVBox G.PackGrow 3
            return (mainBox, optionFrames !! 1, mVBox)
        -- get road classes
        getRClasses :: IO [String]
        getRClasses  = do
            --let path = "./roadAttributes/roadClasses.txt"
            exists <- doesFileExist roadClasses
            if exists then
                L.readFile roadClasses >>=  return . map L.unpack .  L.lines
            else
                return []
        -- showAlready analysed
        showAlreadyAnal ::  G.TextBuffer -> IO ()
        showAlreadyAnal tbuff = do
            let resultsFile = "./SectionResults/results.txt"
            exists <- doesFileExist resultsFile
            if exists then do
                lists <- liftM (map L.unpack . nub  . L.lines) $  L.readFile resultsFile
                mapM_ (updateBufferAtEnd tbuff) lists
            else
                return ()
        -- loading paths
        checkAndUpdatePath :: String -> IO ()
        checkAndUpdatePath str
            | str == "SCANNER" = do
                exists <- doesFileExist (paths !! 0)
                if exists then  do
                    L.writeFile (paths !! 1) =<< L.readFile (paths !! 0)
                    -- analyseSimFileWindow  buffers
                else do
                    -- message that the csv path does not exist and prompt to laod a csv path
                    let string = "SCANNER data not loaded. Would you like to load a SCANNER file?"
                    runDecisionMessageWindow Nothing string $ convertSimFileWindow logger
                    -- runMsgDialog  Nothing string  Error
            | str == "SCRIM" = do
                exists <- doesFileExist (paths !! 2)
                if exists then  do
                    L.writeFile (paths !! 1) =<< L.readFile (paths !! 2)
                    -- analyseSimFileWindow  buffers
                else do
                    -- message that the csv path does not exist and prompt to laod a csv path
                    let string = "SCRIM data not loaded. Would you like to load a SCRIM file?"
                    runDecisionMessageWindow Nothing string $ convertSimFileWindow logger
                    --runMsgDialog  Nothing string  Error
            | otherwise = return ()
            where
                paths  =  ["./SimulationFiles/scannerPath.txt"
                            , "./SimulationFiles/hmdifpath.txt"
                            , "./SimulationFiles/scrimPath.txt"]


--- convenience function to set a message from a list of string
setErrMsg :: [String] -> String
setErrMsg  []      = []
setErrMsg  [a]     = a
setErrMsg [a,b]    = a ++ " nor " ++ b
setErrMsg  (x: xs) = x ++", " ++  setErrMsg xs
------------------
--- a vbox with a lable over the widget
labelOverWidget :: G.WidgetClass widget => String -> widget -> Maybe Int -> IO G.VBox
labelOverWidget string widg mw = do
    box <- G.vBoxNew False 0
    label <- maybe (G.labelNew (Just string)) (flip newWrapedLabel string) mw
    G.boxPackStart box label G.PackNatural 2
    G.boxPackStart box widg  G.PackNatural 0
    return box

--
-- a label of a specified width with the text wraped
newWrapedLabel :: Int -> String -> IO G.Label
newWrapedLabel  width string= do
    label <-  G.labelNew (Just string)
    G.widgetSetSizeRequest label width (-1)
    G.set label [G.miscXalign G.:= 0.1]
    -- G.labelSetJustify label G.JustifyCenter
    G.labelSetLineWrap label True
    return label
--
-----------------------------------------------------------------------------------------------------
------ load simulation file for conversion to mhdif -----------------------------------------------
convertSimFileWindow :: (String -> IO()) -> IO ()
convertSimFileWindow wrtr = do
    win <- G.windowNew
    G.set win [G.windowTitle G.:= "Update Source Files", G.windowResizable G.:= True,
               G.windowDefaultWidth G.:= 500, G.windowDefaultHeight G.:= 100]
    mainBox <- G.vBoxNew False 3
    G.set win [G.containerChild G.:= mainBox]
    -- entry with the path of the simulation file
    [fileEntry, yrEntry] <- sequence $ replicate 2 G.entryNew
    parmBox <- labelLeftofWidget "Enter start year: " yrEntry True
    --
    mapM_ (\ent -> do
                G.entrySetWidthChars ent 6
                G.entrySetMaxLength ent 6) [yrEntry]
    (combo,cBox) <- comboBoxWithLeftLabel "Choose a condition: "

    G.boxPackEnd parmBox cBox G.PackGrow 3
    --
    {- roadNamesPath
    scannerPath = "./SimulationFiles/scannerPath.txt"
    scrimPath = "./SimulationFiles/scrimPath.txt"
    csvPath = "./SimulationFiles/hmdifpath.txt"
    --}
    -------------------------------------------------
    let paths  = ["./SimulationFiles/csvPath.txt" -- hmdifpath
                  ,"./SimulationFiles/scannerPath.txt"
                  , roadNamesPath --
                  -- , "./SimulationFiles/SectionAttributePath.txt"
                  , "./SimulationFiles/scrimPath.txt"]
    pathsRef <- newIORef  ""
    --data PathType = HMDIFF | RoadNames | Simulation | SectionAttribute deriving (Eq , Show)
--disp PathType -> String
--disp HMDIFF  =   RoadNames , SectionAttribute, Simulation
    let ptypes = ["CSV" , "SCANNER HMDIF" ,  "Road Names" , "SCRIM HMDIF"]
    let buttonNames = [" Load file ", " Convert ", " Close "]
    [loadButt,convetButt, cancelButt]  <- mapM G.buttonNewWithLabel  buttonNames
    --- options
    --let sectAttributes = "./roadAttributes/SECTION_ATTRIBUTES.txt"
    -- let roadFile = "./roadAttributes/roads.txt"
    dummy <- G.radioButtonNewWithLabel "Load File"
    fileButtons <-  mapM (G.radioButtonNewWithLabelFromWidget dummy) ptypes
    let radios =  fileButtons
    let  [simOpt , scannerOpt,roadAttributsOpt,scrimOpt] = fileButtons
    --let buttLs ps rs = [p | (p,r) <-  zip ps rs,  let tt = unsafePerformIO $ G.toggleButtonGetActive r, tt == True]

    let buttLs ps rs  = map (\(a,b) -> (a, G.toggleButtonGetActive b)) $ zip ps rs
    let updateBrowseButton rbutt = (G.on rbutt G.toggled $ do
                                        active <- G.toggleButtonGetActive rbutt
                                        if active then
                                            case (findIndex ((== rbutt) . snd) $ zip ptypes radios) of
                                                Nothing -> return ()
                                                Just i -> do
                                                    modifyIORef pathsRef (\_ -> paths !! i )
                                                    --print (paths !! i)
                                                    let blabel a =  "Load "++ a ++ " File"
                                                    G.buttonSetLabel loadButt $ blabel (ptypes !! i)
                                                    --print =<< readIORef pathsRef
                                                    ---
                                                    if rbutt == simOpt then  do
                                                        G.widgetSetSensitive parmBox active
                                                        G.buttonSetLabel convetButt " Convert "
                                                    else do
                                                        G.widgetSetSensitive parmBox False
                                                        G.buttonSetLabel convetButt " Ok "--
                                        else return ()
                                       )   >> return ()

    -- mapM_ (G.radioButtonSetGroup simOpt) (tail fileButtons) -- simOpt
    mapM_ updateBrowseButton radios
    -------------------------------------------------
    mapM_ (G.comboBoxAppendText combo . show) conditions
    -----
    --- top of the box
    G.hBoxNew False 3  >>= \hbox -> do
        sep <- G.hSeparatorNew
        ---
        hbox2 <- tableWithWidgetList False  (fileButtons) True 4 -- G.hBoxNew False 3
        vsep <- G.vSeparatorNew
        ---
        G.boxPackStart hbox loadButt G.PackNatural 1
        G.boxPackStart hbox fileEntry G.PackGrow 1
        ---
        G.boxPackStart mainBox hbox2 G.PackNatural 2
        G.boxPackStart mainBox sep G.PackNatural 5
        G.boxPackStart mainBox hbox G.PackNatural 3

        G.on loadButt G.buttonActivated $ do
           fileBrowser (Just fileEntry) >>= G.dialogRun  >> return ()
    -- bottom of the box
    pregressPane <- G.textBufferNew Nothing
    inputSpace <-  G.textViewNewWithBuffer pregressPane
    --updateBufferAtEnd :: G.TextBuffer -> String -> IO()
    frame <- G.frameNew
    G.hBoxNew False 3  >>= \hbox -> do
        G.containerAdd frame inputSpace
        [leftBox, rightBox] <- sequence $ replicate 2 (G.vBoxNew False 2)
        mapM_ (\b -> G.boxPackStart rightBox b G.PackNatural 2) [convetButt, cancelButt]
        G.boxPackStart leftBox parmBox  G.PackNatural 1
        G.boxPackStart leftBox frame  G.PackGrow 3
        G.boxPackStart hbox leftBox G.PackGrow 2
        G.boxPackStart hbox rightBox G.PackNatural 2
        G.boxPackStart mainBox hbox G.PackGrow 5
    -- handles
    G.on convetButt G.buttonActivated $ do
        path <- G.entryGetText fileEntry
        let rpath    = (reverse path)
        let fileName = takeWhile (/= '/') rpath
        if (null fileName) || (not $ isValid path) then
            runMsgDialog  Nothing "The path entered is invalid"  Error
        else do
            simulation <- G.toggleButtonGetActive simOpt
            if simulation then do
                index <- G.comboBoxGetActive combo
                if index > 0 then do
                    year <- G.entryGetText yrEntry
                    case bs2Int' (L.pack year) of
                        Nothing ->
                            runMsgDialog  Nothing "The year entered is invalid "  Error
                        Just n  ->
                            if n < 1000 then
                                runMsgDialog  Nothing "The year entered is invalid "  Error
                            else -- do
                                myTry $ do
                                    let newYear = "12/04/"++year
                                    outPathFile <- readIORef pathsRef
                                    if (not $ null outPathFile) then do
                                    -- print outPathFile
                                        mHmdifOK <- csv2HMDIF_aux path outPathFile (conditions !! index) newYear
                                        case mHmdifOK of
                                            Nothing -> do
                                                let outFile = (takeWhile (/= '.') (reverse fileName)) ++ ".hmd"
                                                let m1 = "HMDIF file successfully created\n"
                                                let m2 = "output written to " ++ outFile
                                                mapM_ (updateBufferAtEnd pregressPane) [m1,m2]
                                            Just str ->
                                                runMsgDialog Nothing str  Error
                                    else  do
                                        let str = "You did not select a valid path type to load."
                                        runMsgDialog (Just "No path seleted") str  Error
                else
                    runMsgDialog  Nothing "Please select a condition for the HMDIF file. "  Error
            -----
            else do
                let sel bs = [a | (a,b) <- bs, b]
                sel <- liftM sel . mapM liftP $ buttLs ptypes radios
                --
                if (not $ null sel) then do
                    let hmtypes = ["SCANNER HMDIF" ,  "SCRIM HMDIF"]
                    let selected = (sel !! 0)
                    --print sel
                    if selected `elem` (tail ptypes) then do --  ,  "]
                    --- mapM G.toggleButtonGetActive
                        --print selected
                        let ending = dropWhile (/= '.') (reverse fileName)
                        --print ending
                        let end    = if selected `elem` hmtypes then ".hmd" else ".txt"
                        if ending /= end then do
                            let message = "The path entered is not correct. Select a .hmd file for\
                                           \ HMDIF or a .txt file for Road Names."
                            runMsgDialog (Just "Incorrect File Selected")  message  Error
                        else do
                            -- need to do some studd to see if the hmdif was a scrim or acanner file and clear the defects folder
                            -- and the results for the file selected
                            let nodefect     = L.pack $ filter (not . isSpace) selected
                            let m1 = selected ++ " file at " ++ path ++ " \nsuccessfully loaded.\n"
                            outPathFile <- readIORef pathsRef
                            if (not $ null outPathFile) then do
                                L.writeFile outPathFile (L.append nodefect (L.cons ',' (L.pack path)))
                                updateBufferAtEnd pregressPane m1
                                if end == ".txt" then do
                                    --wrt <- ioLogger
                                    forkIO $ setRoadClasses wrtr
                                    return ()
                                else
                                    return ()
                                --removeDirectory defectsDir
                            else do
                                let str = "You did not select a path type to load."
                                runMsgDialog (Just "No path selcted") str  Error

                    else
                        runMsgDialog Nothing "Unknown Error"  Error
                else do
                    let str = "You did not select a path type to load."
                    runMsgDialog (Just "No path selcted") str  Error
    G.on cancelButt G.buttonActivated $ G.widgetDestroy win
    G.widgetShowAll win
        where
            liftP :: Monad m =>  (a, m b) -> m (a,b)
            liftP (a, b) = do
                bb  <- b
                return (a,bb)
---
