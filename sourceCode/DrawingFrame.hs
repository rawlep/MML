{-# LANGUAGE BangPatterns #-}

module DrawingFrame (fileBrowser,isValid,reds, blues -- getControlBox,getWindowContents,
                     ,runAboutDialog,openFeedBackForm,openProgessWindow
                     --,progressDialogWithTrackListAndInfoButt -- multChainsWindow,makeScrolledEntryArea,
                     ,comboText,topViewBox,mkDnaTable -- mkDnaBase,
                     ,getCanvasDisplay,mkDnaDisplayFrame -- visualizationBox,
                     ,firstTimeInstallation , updateSourceFiles
                     --openProgessWindow,updateDrawingArea,getDrawingCanvas
                     --,disp,getDrawPtAbiscaVals
                     --,errorAnalysisAndForcastRow
                     --,ControlBox (CBox,controlBox,analysisButtons,nextStuff,drawingOptions)
                     -- abissica,values) -- ,otherStuff
                     ,updateDrawingArea,getDrawingCanvas -- ,vals2drwRef
                     ,mkProgressionGroupList,openStartupWindow
                     ,colors,colours,frameLabel -- gColors,
                    ) where
{--
This module constructs a drawing frame which cotains a
a drawing canvas and controls for updating graphs on the canvas.
Either one or two canvases can be packed into the frame, depending on
whether the user requests to view an adjacent
section (using the MMLRPR).

Since the number of frames, and the contents of hte control pane
(when there are two convases, the controls govern both canvases), to
pack are determined by the user-controlled actions, the the is passed
into the construciton function as an IORef parameter
--}

import Prelude hiding (tan)
import qualified Graphics.UI.Gtk as G
import qualified Data.ByteString.Lazy.Char8 as L (unpack,dropWhile,pack,concat,writeFile,
                                                  snoc,cons,append,lines, readFile,intercalate)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM) -- , liftM2)
import Control.Concurrent (forkIO,killThread)
import Control.Exception (SomeException)
import Data.Maybe (isJust,maybe,listToMaybe)
import Data.List (delete, find, findIndex, deleteBy,maximumBy, nub)
import Data.Char (isDigit)
import System.IO.Unsafe (unsafePerformIO)
import Data.Ord (comparing)
import System.Directory (doesFileExist) -- , doesDirectoryExist,createDirectory)
import Data.IORef
--import Data.Colour.SRGB
------------------------Data.Colour.SRGB---------------------------------------------------------------------------
-- graphing
import Numeric.LinearAlgebra hiding (find)
--import Graphics.UI.Gtk hiding(Circle,Cross)
import Control.Monad.Trans
--import Graphics.Rendering.Plot -- as P
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
--import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Data.Colour (black, opaque)
import Data.Colour.Names
-- import Data.Colour.Internal
--import Data.Colour.RGBSpace ( RGB)
--import Numeric.GSL
---------------------------------------------------------------------------
import Time
import Data.Time.Clock
import Data.Time.Calendar
---------------------------------------------------------------------------
import GlobalParameters --(EnabledLists,modifyNextChainClick)
import NiceFork
--import QueryDialog (runQueryDialog )
--import ProcessFile (display,intercalate, replaceAt,getLicense)
--import HMDIFPrelude (Defect (..) ,scode2align , typesAndDetails,getTypeFromDetail,conditions,nthComma')
--import ReadFromDataFiles1 (readRawOutput,bs2HMDIF2bs_aux)
import RprMix2 (oMod,alph, sigD, sigE,vunStdNorm,vmean,vstDiv)
import ListStats  (mean)
import LMisc (myTake,trendLine,lastN)
import Parallel
---------------------------------------------------------------------------
colours = [blue
 ,brown
 ,burlywood
 ,coral
 ,cornflowerblue
 ,crimson
 ,cyan
 ,darkgoldenrod
 ,darkgray
 ,darkgreen
 ,darkkhaki
 ,darkmagenta
 ,darkolivegreen
 ,darkorange
 ,darkorchid
 ,darkred
 ,darksalmon
 ,darkseagreen
 ,darkslateblue
 ,darkviolet
 ,deeppink
 ,deepskyblue
 ,dimgray
 ,dodgerblue
 ,firebrick
 ,forestgreen
 ,fuchsia
 ,gainsboro
 ,gold
 ,goldenrod
 ,gray
 ,green
 ,greenyellow
 ,honeydew
 ,hotpink
 ,indianred
 ,indigo
 ,ivory
 ,khaki
 ,lavender
 ,lavenderblush
 ,lawngreen
 ,lemonchiffon
 ,lightblue
 ,lightcoral
 ,lightcyan
 ,lightgoldenrodyellow
 ,lightgreen
 ,lightpink
 ,lightsalmon
 ,lightseagreen
 ,lightskyblue
 ,lightslategray
 ,lightsteelblue
 ,lightyellow
 ,lime
 ,limegreen
 ,linen
 ,magenta
 ,maroon
 ,mediumaquamarine
 ,mediumblue
 ,mediumorchid
 ,mediumpurple
 ,mediumseagreen
 ,mediumslateblue
 ,mediumspringgreen
 ,mediumturquoise
 ,mediumvioletred
 ,midnightblue
 ,mintcream
 ,mistyrose
 ,moccasin
 ,navajowhite
 ,navy
 ,oldlace
 ,olive
 ,olivedrab
 ,orange
 ,orangered
 ,orchid
 ,palegoldenrod
 ,palegreen
 ,paleturquoise
 ,palevioletred
 ,papayawhip
 ,peachpuff
 ,peru
 ,pink
 ,plum
 ,powderblue
 ,purple
 ,red
 ,rosybrown
 ,royalblue
 ,saddlebrown
 ,salmon
 ,sandybrown
 ,seagreen
 ,seashell
 ,sienna
 ,silver
 ,skyblue
 ,slateblue
 ,slategray
 ,snow
 ,springgreen
 ,steelblue
 ,tan
 ,teal
 ,thistle
 ,tomato
 ,turquoise
 ,violet
 ,wheat
 ,yellow
 ,yellowgreen]
------------------------------------------------------------------------------------
-- colours from GTK ranging from red to blue
-- color2gcolor :: (Ord a, Floating a) => Colour a -> G.Color a
--color2gcolor col = G.Color (a * 257) (b * 257) (c * 257)
--    where
--        RGB a b c = toSRGB col

colors = [ G.Color (257 * a)  (257 * b)  (258 * c) |
                    a <- [0  .. 255]
                    , b <- [1 .. 240]
                    ,c <- [60 .. 255]  ]

reds = [(255,0,0),
        (255,60,65),
        (255,115,95),
        (255,135,120),
        (255,175,155),
        (255,200,185),
        (255,210,210),
        (255,235,235),
        (255,245,245),
        (255,255,255)
        ]
---
blues = [(255,255,255),
         (245,245,255),
         (235,235,255),
         (210,210,255),
         (185,192,255),
         (150,176,255),
         (100,145,255),
         (74,105,255),
         (48,70,255),
         (0,0,255)
        ]
--}
-- convience runction which returns the active sting in a combobox or the
-- empty string if none is active (or if the active string is a empty string)
comboText :: G.ComboBox -> IO String
comboText combo = G.comboBoxGetActiveText combo >>= return . maybe "" id

--------------------------------------------------------------------------------------
openFeedBackForm = do
    logDia <- G.dialogNew
    --G.set logDia [ G.windowTitle G.:= "Comments or Queries",
    --               G.windowDefaultWidth G.:= 350, G.windowDefaultHeight G.:= 300]
    G.dialogAddButton logDia " Close " G.ResponseOk
    G.set logDia [G.windowTitle G.:= "Pavement Data Preparation and Analysis",G.windowTitle G.:= "Guide",
                  G.windowDefaultWidth G.:= 850, G.windowDefaultHeight G.:= 400]
    G.windowMaximize logDia
    upBox <- G.dialogGetUpper logDia
    nameBox <- G.hBoxNew False 1
    ----
    (scrwin,tbuff) <- makeScrolledEntryArea VScroll True True
    G.textBufferSetText tbuff =<< (liftM L.unpack $ L.readFile "./Licenses/MML2DS_User_Guide.txt")
    inputFrame <- G.frameNew
    --G.scrolledWindowAddWithViewport scrwin inputSpace
    inputLabel <- G.labelNew (Just "Enter your questions, comments or suggestions in the space \
                                    \ below. Please put each comment,etc in a new line.")
    nameLabel <- G.labelNew (Just " Your name (optional): ")
    nameSpace <- G.entryNew
    hsep <- G.hSeparatorNew
    --
    G.boxPackStart nameBox nameLabel G.PackNatural 0
    --G.boxPackStart nameBox nameSpace G.PackGrow 4
    G.containerAdd inputFrame scrwin
    G.boxPackStart upBox nameBox G.PackNatural 2
    G.boxPackStart upBox hsep G.PackNatural 2
    G.boxPackStart upBox inputLabel G.PackNatural 4
    G.boxPackStart upBox inputFrame G.PackGrow 4
    G.widgetShowAll upBox -- inputSpace
    ans <- G.dialogRun logDia
    if ans == G.ResponseOk then
            -- save to a file
        G.widgetDestroy logDia
    else
        G.widgetDestroy logDia
    --return bBox

-----------------------------------------------------------------------------------
------------ some Controls
----------------------------------------------------------------------------------

-- first time installation
data PathType = HMDIFF | RoadNames | Simulation | SectionAttribute deriving (Eq , Show)
--disp PathType -> String
--disp HMDIFF  =   RoadNames , SectionAttribute, Simulation
pathtypes = [HMDIFF ,  RoadNames , SectionAttribute, Simulation]
buttLs rs = [p | (p,r) <-  zip pathtypes rs,  unsafePerformIO $ G.toggleButtonGetActive r]
--
firstTimeInstallation :: Maybe PathType -> IO () -> IO () -- G.Window
firstTimeInstallation mPtType nextProcess = do
    let pathsFile = "./FilePaths/pathsFile.txt"
    filesRef <-  newIORef mPtType -- (if update then [HMDIFF ,  RoadNames , SectionAttribute, Simulation] else [HMDIFF ,  RoadNames , SectionAttribute])
    typesRef <-  newIORef [HMDIFF ,  RoadNames , SectionAttribute, Simulation]
    --G.set label [G.labelWrap G.:=  True]
    havePaths <- firstTimeInstallation_aux filesRef pathsFile
    case havePaths of
        Nothing ->  nextProcess
        Just (radios,browseButton,fileEntry, msgEntry) -> do
            let text = maybe "Update source files for  analysis. " (\a -> "Update a " ++ show a ++ "file for analysis" ) (listToMaybe $ buttLs radios)
            label <- G.labelNew (Just text)
            modifyIORef filesRef (\_ ->  listToMaybe $ buttLs radios)
            -----------------------------------------------
            mdia <- myDialogNew
            simFrame <- G.frameNew
            G.containerAdd simFrame msgEntry
            -- adding widgets to the window
            myDialogAddWidget mdia label
            myDialogAddWidget mdia =<< tableWithWidgetList False  radios True 3
            myDialogAddWidget mdia =<<  widgetLeftWifget True browseButton fileEntry
            G.boxPackStart (upper mdia) simFrame G.PackGrow 2
            [continueButton, cancel] <- mapM G.buttonNewWithMnemonic ["_Continue","_Quit"]
            mapM_ (myDialogAddButton mdia) [continueButton, cancel]
            --
            let titleStr = "First Time Installation"
            modifyIORef (title mdia) (\_ -> titleStr)
            myDialogShow mdia
            --
            G.on cancel G.buttonActivated $  G.widgetDestroy (myDia mdia) >> G.mainQuit
            (G.on continueButton G.buttonActivated $  do
                exists <- doesFileExist pathsFile
                let dummy  = (L.pack  "null,null")
                let noPaths = replicate 4 dummy
                -- enter a loop
                -- fpaths <- liftM L.lines $ L.readFile pathsFile
                --let buttLs = [show p | (p,r) <-  zip pathtypes radios,  unsafePerformIO $ G.toggleButtonGetActive r]
                paths <- if exists then  liftM L.lines $ L.readFile pathsFile else return noPaths
                entry <- G.entryGetText fileEntry -- >>=
                upd <- updatePath entry paths radios
                case upd of
                    Just newPaths -> do
                      --   case (findIndex (== dummy) npth) of
                        let toWrite = L.intercalate (L.pack "\n") newPaths
                        forkIO $ L.writeFile pathsFile toWrite
                        --- chek to update the otehr types
                        print toWrite
                        G.widgetDestroy (myDia mdia)
                        nextProcess
                    Nothing  ->
                        runMsgDialog (Just "Missing Entry") ("You did not enter a path for the " )  Error

                ) >> return ()
    where
        updatePath entry paths radios = do
           if  (not . null) entry then  do
                (year,month,day) <- getCurrentTime >>= return . toGregorian . utctDay
                let today = L.pack (show month ++ "/" ++ show day++"/"++show year)
                let aDate = L.snoc today ','
                let entry1 = aDate `L.append` (L.pack entry)
                let newPaths =  [np| (p,r) <- zip paths radios
                                         , let tt = unsafePerformIO $ G.toggleButtonGetActive r
                                         , let np = if tt  then  entry1 else p  ]

                return $ Just newPaths
            else return Nothing


firstTimeInstallation_aux  updateRef pathsFile = do
    -- chekc paths file [G.RadioButtons] G.Button G.Entry G.Entry
    exists <- doesFileExist pathsFile
    pRef <- readIORef updateRef
    if exists && (pRef == Nothing) then
        return Nothing --- nextProcess
    else do
        let rnames = map show pathtypes
        radios <- makeRadioButtonGroupFromStrings  rnames
        -- let buttLs = [show p | (p,r) <-  zip pathtypes radios,  unsafePerformIO $ G.toggleButtonGetActive r]
        browseButton <- G.buttonNewWithMnemonic (maybe "Load " (\a -> "Load " ++  show a ++ " File") (listToMaybe $ buttLs radios))
        let updateBrowseButton rbutt = (G.on rbutt G.toggled $ do
                                           -- togg <- G.toggleButtonGetActive rbutt
                                                modifyIORef updateRef (\_ -> listToMaybe $ buttLs radios)
                                                case (findIndex (== rbutt) radios) of
                                                    Nothing -> return ()
                                                    Just i -> do
                                                        let blabel a =  "Load "++ a ++ " File"
                                                        G.buttonSetLabel browseButton $ blabel (rnames !! i)
                                            --else return ()
                                       )   >> return ()

        mapM_ updateBrowseButton radios
        [fileEntry, msgEntry] <- sequence $ replicate 2 G.entryNew
        G.on browseButton G.buttonActivated $ do
           fileBrowser (Just fileEntry) >>= G.dialogRun  >> return ()
        --G.widgetSetSensitive (radios !! 3) update
        -- read the paths file
        --(boxes, entries) <- liftM unzip $ mapM createButtForSearchPath [Just a | a <- pathtypes] -- pathsFile
        --G.widgetSetSensitive (boxes !! 3) update
        --table <- tableWithWidgetList True  boxes True 6
        --continueButton <- G.buttonNewWithMnemonic "_Continue"
        --processDialogNew :: G.WidgetClass widget =>  Maybe widget -> [(G.Button , IO ())] -> IO G.Dialog
        return $ Just (radios,browseButton,fileEntry, msgEntry)

--- this code is repeated from controlFlow
createButtForSearchPath ::  Maybe PathType -> IO (G.HBox,G.Entry)
createButtForSearchPath   pathType = do --
    fileEntry <- G.entryNew
    let
    -- data PathType = HMDIFF | RoadNames | Simulation | SectionAttribute deriving (Eq , Show)
    let buttonName = case pathType of
                        Just HMDIFF -> "Load _HMDIF File: "
                        Just RoadNames -> "Load _Roads File: "
                        Just SectionAttribute -> "Load _Names File: "
                        Just Simulation -> "Load _Sumilation File: "
                        Nothing          -> "Load File"
    browseButton <- G.buttonNewWithMnemonic buttonName
    buttonBox <-  widgetLeftWifget True browseButton fileEntry  -- G.hBoxNew False 1
    G.widgetSetSizeRequest browseButton 100  (-1)
    G.on browseButton G.buttonActivated $ do
           fileBrowser (Just fileEntry) >>= G.dialogRun  >> return ()
    case pathType of
        Just Simulation -> do
            optBox <- G.vBoxNew False 0
            G.boxPackStart optBox buttonBox G.PackNatural 2
            --
            mainBox <- G.hBoxNew False 0
            G.boxPackStart mainBox optBox G.PackGrow 0
            return (mainBox,fileEntry)
        otherWise ->
            return  (buttonBox, fileEntry)
-- open startup  window

updateSourceFiles :: IO ()
updateSourceFiles = do
    -- let (font, back) =
    let pathsFile = "./FilePaths/pathsFile.txt"
    let hmdifFile = "./FilePaths/hmdif.txt"
    let simulationFile = "./FilePaths/simulation.txt"
    let schemeFile = "./FilePaths/schemeFile.txt"
    let secationAttrbsFile = "./FilePaths/secationAttFile.txt"
    exists <- doesFileExist pathsFile
    ptRef <- newIORef Nothing
    if exists then do
        --pathsInFile <- liftM L.lines $ L.readFile pathsFile
        hmd <- G.radioButtonNewWithLabel "HMDIF File"
        rbuts <- mapM (G.radioButtonNewWithLabelFromWidget hmd) ["Simulation File","Roads File","Names File"]
        rbuttsTable <- tableWithWidgetList  False (hmd : rbuts) True 4
        label <- G.labelNew (Just "Enter File")
        --
        let ontoggle butt = G.on butt G.toggled $ do
                                bt <- G.toggleButtonGetActive butt
                                if bt then do
                                    case find ((== butt) . fst) (zip (hmd : rbuts) pathtypes) of
                                        Nothing -> return ()
                                        Just (_,bt) -> do
                                            --let mPT = listToMaybe [pt | (tf,pt) <- zip (hmd : rbuts) pathtypes
                                            --                    , butt == tf]
                                            --G.labelSetText label ("Enter " ++ maybe "File " ((++ "File ") . show)  mPT ++ "to update")
                                            G.labelSetText label ("Enter "++  show bt ++ " file to update")
                                            writeIORef  ptRef (Just bt)
                                else return ()
                                                --             , butt == tf ]
        mapM_ ontoggle (hmd : rbuts)
        --undate the label entry everytime the toggle changes
        ---
        (hbox, entry) <- createButtForSearchPath Nothing
        --- creat the dialog
        dia <- G.dialogNew
        upper <- G.dialogGetUpper dia
        lower <- G.dialogGetActionArea dia
        G.boxPackStart upper rbuttsTable G.PackNatural 5
        sep <- G.hSeparatorNew
        G.boxPackStart upper sep G.PackNatural 1
        G.boxPackStart upper label G.PackNatural 2
        G.boxPackStart upper hbox G.PackGrow 2
        updateButton <- G.buttonNewWithMnemonic "_Update"
        --
        G.boxPackEnd lower updateButton G.PackNatural 3
        G.on updateButton G.buttonActivated $  do
            mPathType  <- readIORef  ptRef
            path <-  G.entryGetText entry
            if null path then do
                --mPathType  <- readIORef  ptRef
                let str = maybe "" (\a -> "the " ++ show a ++ " file")  mPathType
                runMsgDialog Nothing ("You did not enter a path to update " ++ str)  Error
            else do
                case mPathType of
                    Nothing -> runMsgDialog Nothing ("Something's strange: the toggle selection did not update")  Error
                    Just HMDIFF -> L.writeFile hmdifFile (L.pack path) >> G.widgetDestroy dia
                    Just RoadNames -> L.writeFile schemeFile (L.pack path) >> G.widgetDestroy dia
                    Just Simulation -> L.writeFile simulationFile (L.pack path) >> G.widgetDestroy dia
                    Just SectionAttribute -> L.writeFile secationAttrbsFile (L.pack path)>> G.widgetDestroy dia
                        -- HMDIFF | Road | Simulation | SectionAttribute
                        --Case
                        {-
                        case findIndex (== typ) pathtypes of
                            Nothing ->
                                runMsgDialog ("Something's fishy: couldn't find the path in 'pathtypes' ")  Error
                            Just i -> do
                                pathsInFile <- liftM L.lines $ L.readFile pathsFile
                                (year,month,day) <- getCurrentTime >>= return . toGregorian . utctDay
                                let today = L.pack (show month ++ "/" ++ show day++"/"++show year)
                                let aDate = L.cons ',' (L.snoc today '\n')
                                let newPath = L.append (L.pack path) aDate
                                let nwBsp  =  L.concat $ replaceAt i newPath pathsInFile
                                --}
                                {-- need to fork this
                                if i > 0 then do
                                    if i == 1 then
                                        createSchemeFile path (pathsInFile !! 2) True
                                    else
                                        createSchemeFile (pathsInFile !! 1) path True
                                else return  ()
                                --}
                                --
        bb <- G.dialogAddButton dia "Cancel" G.ResponseClose
        G.on bb G.buttonActivated $  G.widgetDestroy dia -- process
        --
        let title = "Update Source Files"
        G.set dia [G.windowTitle G.:=  title , G.windowResizable G.:= False]
        G.widgetSetSizeRequest dia 700  (-1)
        G.widgetShowAll dia
    else
        runMsgDialog Nothing ("Something went wrong: there are no files to update")  Error



-----------}
topViewBox :: G.Window -> String -> IO (G.HBox)
topViewBox mainWindow string = do
    mainBox <- G.vBoxNew False 0
    [mHBox, lblBox,bottomBox] <- sequence . replicate 3 $ G.hBoxNew False 6
    return mHBox
    -- ++"\n")
{--
--- this is really for visualising raw data
visualizationBox :: Int -> String ->  IO ( G.VBox)
visualizationBox i outfile = do
    mainBox <-  G.vBoxNew False 0
    return mainBox

-- getting the title for a window : G.windowGetTitle
--------------------------------------------------------------------}
---                                 ABOUT DIALOG
------------------------------------------------------------------------------------
-- runs the about dialog
runAboutDialog :: IO ()
runAboutDialog = do
    license <- getLicense "./Licenses/other_license.txt" -- "GNU_PUBLIC_LICENSE.txt"
    image <- G.pixbufNewFromFileAtSize "./Logos/nottDevonLogo.png" 400 150
    aboutDia <- G.aboutDialogNew
    G.set aboutDia [G.aboutDialogProgramName G.:= "MML-2DS",
                    G.aboutDialogComments G.:= "A System for the Preparation and Investigation of road\
                                                \ condition data for Analysis and Deterioration modelling\
                                                \ based on the MML metric.",
                    G.aboutDialogVersion G.:= "1.0.1",
                    G.aboutDialogCopyright G.:= "Copyright 2012 Devon County Council and University of Nottingham",
                    G.aboutDialogAuthors G.:= ["Rawle Prince"],
                    G.aboutDialogLicense G.:= Just license,
                    G.aboutDialogWebsiteLabel G.:= "http://www.nottingham.ac.uk/~evzncpe/ntec.htm",
                    G.aboutDialogLogo G.:= Just image,
                    G.aboutDialogWrapLicense  G.:= True ]
    ans <- G.dialogRun aboutDia
    G.dialogResponse aboutDia ans -- get a response
    G.widgetDestroy aboutDia


------------------------------------------------------------------------------------
---         Start-up Window
-----------------------------------------------------------------------------------
openStartupWindow ::  G.Window ->  IO G.Dialog
openStartupWindow mainWindow  = do --
    win <- G.dialogNew
    --G.onDestroy win G.mainQuit
    --G.onDelete win (\_ ->  G.mainQuit >> return True )
    ----
    G.widgetSetSizeRequest win 720 380
    G.set win [G.windowResizable G.:= False]
    license <-  G.imageNewFromFile "./Licenses/other_license.txt" -- "GNU_PUBLIC_LICENSE.txt"
    image <- G.imageNewFromFile "./Logos/nottDevonLogo.png" -- 400 150
    quitRef <- newIORef Nothing
    --imageBox <- G.hBoxNew False 0
    --G.set imageBox [G.containerChild G.:= image]
    logPaneOption <- G.checkButtonNewWithLabel "Hide Log Pane"
    upBox <- G.dialogGetUpper win
    bottomBox <- G.dialogGetActionArea win
    --makeScrolledEntryArea ::  TScroll -> Bool -> Bool -> IO (G.ScrolledWindow , G.TextBuffer )
    (swin, ent) <- makeScrolledEntryArea VScroll False False
    -- set the text of the display area
    getLicense "./Licenses/other_license.txt" >>= G.textBufferSetText ent
    G.boxPackStart upBox image G.PackNatural 1
    G.boxPackStart upBox swin G.PackGrow 1
    [disagreeButt ,agreeButt] <- mapM G.buttonNewWithLabel [" Disagree ", " Agree "]
    mapM_ (\b -> G.boxPackEnd bottomBox b G.PackNatural 1) [disagreeButt ,agreeButt]
    G.on agreeButt G.buttonActivated $ do
         G.widgetShowAll mainWindow >> G.widgetDestroy win --
    G.on disagreeButt G.buttonActivated $ do
        let message = "You have not agreed with the License agreement and cannot continue with\
        \ this application. Do you wish to reconsider?"
        --runMsgDialog  str  Info
        dia <- G.messageDialogNew Nothing [G.DialogDestroyWithParent] G.MessageQuestion G.ButtonsYesNo message
        ans <- G.dialogRun dia
        if ans == G.ResponseYes then do
            G.widgetDestroy dia
            -- modifyIORef quitRef (\_ -> Just win)
        else
            -- modifyIORef quitRef (\_ -> Nothing) --
            G.mainQuit
    return win


------------------------------------------------------------------------------------
-- file Chooser dialog
------------------------------------------------------------------------------------
fileBrowser :: Maybe G.Entry -> IO (G.Dialog)
fileBrowser fileEntry = do
    fch <- G.fileChooserWidgetNew G.FileChooserActionOpen
    dia <- G.dialogNew
    G.widgetSetSizeRequest dia 600 300
    upBox <- G.dialogGetUpper dia
    bottomBox <- G.dialogGetActionArea dia
    let filters = [("*.*","All Files"),("*.txt" , "Text Files"),("*.hmd","HMDIF Fies"),("*.csv","CSV Fies") ]
    --
    mapM_ (addFiters fch) filters
    [okButt ,cancelButt] <- mapM G.buttonNewWithLabel [" Ok ", " Cancel "]
    G.on cancelButt G.buttonActivated $ do
        -- maybe (return ()) (\fe -> G.entrySetText fe "") fileEntry
        G.widgetDestroy dia
    G.on okButt G.buttonActivated $ do
       G.fileChooserGetFilename fch >>= \mfp ->
            case maybe2 mfp fileEntry of
                Nothing -> return () --
                Just (string,fe) -> do
                    if isValid string then do
                        G.entrySetText fe string
                        G.widgetDestroy dia
                    else do
                        let errMsg = "The chosen file is not a .txt file or a .hmd\
                                     \ file, and is not valid. You can apply the filters\
                                     \ below to reduce the options. "
                        runMsgDialog Nothing errMsg Error
    G.boxPackStart upBox fch G.PackGrow 2
    mapM_ (\b -> G.boxPackEnd bottomBox b G.PackNatural 4) [cancelButt,okButt ]
    --
    G.widgetShowAll dia
    return dia
    where
        addFiters fch (pattern, filterName) = do
             hsfilt <- G.fileFilterNew
             G.fileFilterAddPattern hsfilt pattern
             G.fileFilterSetName hsfilt filterName
             G.fileChooserAddFilter fch hsfilt

-- valid file extensions
isValid str  = any (== (lastN 4 str)) [".txt",".hmd",".TXT",".HMD",".csv",",CSV"]

------------------------------------------------------------------------------------
--                      DRAWING ON THE CANVAS
------------------------------------------------------------------------------------
-- getCurrSegRef ::  IO String
getDrawingCanvas :: Maybe (IORef DrawRef) ->  IO G.DrawingArea
getDrawingCanvas  menbRef = do
    canvas <- G.drawingAreaNew
    G.widgetModifyBg canvas G.StateNormal (G.Color 65535 65535 65535)
    G.widgetAddEvents canvas [G.Button1MotionMask,G.Button1MotionMask]
    maybe (return canvas)
          (\ref -> do
                G.on canvas G.exposeEvent $ liftIO $  updateDrawingArea canvas ref
                return canvas
          ) menbRef


-----
getCanvasDisplay :: Maybe (IO ()) -> -- possible handle for the button
    Maybe (IORef DrawRef) -> -- for updating the canvas
    String ->  -- label name
    String ->        -- button name
    IO G.HBox
getCanvasDisplay  buttonHandle mDref  labelName  buttonName = do
    mainBox <-  G.vBoxNew False 0
    outBox <- G.hBoxNew False 0
    desFrame <- G.frameNew
    G.set desFrame [G.frameLabel G.:= labelName ]
    G.containerAdd desFrame mainBox
    button <- G.buttonNewWithLabel buttonName
    -- handle button
    maybe (return ()) (\a -> (G.on button G.buttonActivated $ a) >> return ())  buttonHandle
    headerBox <- labelLeftofWidgetE "" 2 button True
    canvas <- getDrawingCanvas mDref
    -- packing
    G.boxPackStart mainBox headerBox G.PackNatural 2
    G.boxPackEnd mainBox canvas G.PackGrow 2
    G.boxPackStart outBox desFrame G.PackGrow 2
    return outBox
---------------
vunStdNorm' vs = toList $ vunStdNorm (vmean vs') (vstDiv vs') vs'
    where vs' = fromList vs
-----------------------------------------------------------------------------------------
--- effecting the actual drawings
updateDrawingArea :: G.DrawingArea -> IORef DrawRef -> IO Bool
updateDrawingArea canvas dref  = do
     drw <- readIORef dref
     let title = caption drw
     let currData = fromTo (currChain drw) (addChain drw) $ zip (drawData drw) (chnLabel drw)
     let ydColors = (zip (map (\(a,b) -> (unzip a , b)) currData) colours)
     --let chart = layout ydColors title (chnLabel drw)
     --print ("addChain is currently: " ++ show (addChain drw))
     let renderable = toRenderable $ layout ydColors title (chnLabel drw) (rslts drw)
     updateCanvas  renderable canvas
---------------
     where
        -- plotting lines
        fun :: Results ->  Double -> Double
        fun  rs = trendLine (rpRpredictions rs) (rprCutsFreq rs)
        ---
        plotTrend mRslts xVals = maybe [] (\rst -> [Left (toPlot $ condition_points xVals (rpRpredictions rst) green "pred")]) mRslts -- trend_aux xVals rst
        trend_aux  xvls rlt = plot_lines_values ^= [[(x,fun rlt x) | x <- [0, 0.2 .. 6] ]  ]
              $ plot_lines_style  .> line_color ^= (opaque  blue)
              $ plot_lines_title ^= "trend"
              $ defaultPlotLines

        --plotting points
        condition_points xs ys col lbl = plot_points_style ^= filledCircles 2 (opaque col)
           $ plot_points_values ^= zip xs ys
           $ plot_points_title ^= (" " ++ lbl)
           $ defaultPlotPoints
           --
        layout vCol title lbls mrs = layout1_title ^= title
            -- (plotTrend mrs (ffst vCol)) <-- need to revisit  this
           $ layout1_plots ^=  (concatMap (plotTrend mrs . ffst) vCol) ++ (concatMap mkLayout1_plots vCol)
           -- $ layout1_lines ^=  (plotTrend mrs (ffst vCol)) -- map (Left . toPlot . uncurry condition_points) . fromTo
           $ defaultLayout1
        -----
        ffst = fst . fst . fst
        ----------- modifiny the draw parameters
        mkLayout1_plots (((xs,ys),lbl), col) = [Left (toPlot  $ condition_points (map (fromIntegral . floor) xs) ys col lbl),
                                                Left (toPlot $ PlotHidden xHide yHide)]
            where
                m   = minimum ys
                my  = maximum ys
                mm  = minimum xs
                mx  = maximum xs
                xHide = [head xss, last xss]
                yHide = [head yss, last yss]
                xss = (mm - 1) : (xs ++ [mx + 1])
                yss = (m - 0.15) : (ys ++ [my + 0.15])
        --mmin = minimum . map minimum
        --mmax = maximum . map maximum
        fromTo from to = myTake to . drop from
        --}

-------------- rendering the canvas -----------------------------------------------------------}
disp :: G.DrawingArea -> ((Int,Int) -> C.Render ()) -> IO Bool
disp  canvas  f  = do
    xy <- liftIO $ G.widgetGetSize canvas
    drWin <- liftIO $ G.widgetGetDrawWindow canvas
    liftIO $ G.renderWithDrawable drWin (f xy)
    return True
 {--
    Just (ot, fn) ->
        writeFigure ot fn xy figure
        modifyIORef enbRef id -- need to change this
 --}
------------------------------------------------------------------------------------------------
--              CREATING A PROGRESSION GROUP
------------------------------------------------------------------------------------------------
-- now make a progression Group box
mkProgressionGroupList :: String -> [DrawRef] -> IO G.ScrolledWindow
mkProgressionGroupList strlabel drfList = do
    -- make the HBox with the labels, etc
    [topBox,tBox] <- sequence . replicate 2 $ G.hBoxNew False 0
    vBox <- G.vBoxNew False 0
    predictButt <- G.buttonNewWithLabel "Forecast"
    lbl <- G.labelNew (Just ("Progression Group: " ++ strlabel))
    -- G.boxPackStart optionsBox chainEntry G.PackNatural 2
    G.boxPackStart topBox lbl G.PackNatural 3
    G.boxPackEnd topBox predictButt G.PackNatural 3
    --
    G.boxPackStart vBox topBox G.PackNatural 0
    G.boxPackStart tBox vBox G.PackNatural 0
    -- handle for predict b
    mapM mkProgressionGroup  drfList >>= flip vScrolledWinWithWgtList VScroll -- .  (tBox :)
    where
        mkTable bb bb1 spc lst = tableWithWidgetList bb lst bb1 spc
        --vScrolledWinWithWgtList
        ---
        mkProgressionGroup :: DrawRef -> IO G.HBox
        mkProgressionGroup drf = do
            canvasBox <- G.hBoxNew False 0
            -- chin
            chains <- mapM (G.labelNew . Just) (chnLabel drf)
            chTable <- tableWithWidgetList True chains True 0
            -- dref and canvas
            canvas <- newIORef drf >>= getDrawingCanvas . Just
            G.boxPackStart canvasBox chTable G.PackNatural 3
            G.boxPackStart canvasBox canvas G.PackGrow 1
            return canvasBox

-------------------------------------------------------------------------------------------------
--- CREATING A DNA DIAGRAM
-------------------------------------------------------------------------------------------------
verticalLabel :: Maybe String -> IO G.Label
verticalLabel mstr = do
    label <- G.labelNew mstr
    G.labelSetMaxWidthChars label 1
    G.labelSetLineWrap label True
    return label


---}
mkDnaTable :: [Results] -> [Int] -> IO G.Table
mkDnaTable rs  cols = do
    tab <- G.tableNew (sum cols) 1 False
    mapM mkDnaRow rs >>= flip (addRows tab) (0:cols)
    where
        addRows table (y:ys) (n:m:xs) = do
            G.tableAttachDefaults table y 0 1 n (m+n)
            addRows table ys ((n+m):xs)
        addRows table  _ _ = return table
        ------
        mkDnaRow :: Results -> IO G.Table
        mkDnaRow   results = do
            table <- G.tableNew 1 (numCols results) False
            add2table table (0 : rprCutsFreq results)
            where
                numCols = (sum . rprCutsFreq)
                --
                add2table :: G.Table -> [Int] -> IO G.Table
                add2table  table (n:m:xs) = do
                    canvas <- getDrawingCanvas Nothing
                    frame <- G.frameNew
                    G.containerAdd frame canvas
                    G.tableAttachDefaults table frame n (m+n) 0 1
                    add2table table ((m+n) : xs)
                add2table    table  _       = return table
------------------------------------------------------------------------------
--  mkResultBox >>= mkDnaDisplayFrame
-- make the outlline for a DNA diagram, along with the canvased and the display of errors
--- need to pass in the buttons and other stuff for the probabilities
mkDnaDisplayFrame :: G.WidgetClass w => Bool -> w ->  G.Table -> IO G.VBox -- G.Table --- G.HPaned -- G.VBox --
mkDnaDisplayFrame  prGroups w  canvasTable = do
    [out, errorBox] <- sequence $ replicate 2 (G.vBoxNew False 1)
    -- errorBox <- G.vBoxNew False 1
    errorFrame <- G.frameNew
    G.containerAdd errorFrame errorBox
    ---
    if (not prGroups) then do
        G.labelNew (Just "Error Above") >>= \label -> do
            G.boxPackStart errorBox label G.PackNatural 0
        aboveTable <- getTableForError 10 True
        belowTable <- getTableForError 10 False
        --
        G.boxPackStart errorBox aboveTable G.PackGrow 0 -- [aboveTable, belowTable]
        sep <- G.hSeparatorNew
        G.boxPackStart errorBox sep G.PackNatural 1
        G.labelNew (Just "Error Below") >>= \label -> do
            G.boxPackStart errorBox label G.PackNatural 0
        G.boxPackStart errorBox belowTable G.PackGrow 0
    else
        return ()
    rgtWg <- G.tableNew 2 23 False -- G.hBoxNew False 1
    -------------------------------------------------------
    G.tableAttachDefaults rgtWg errorFrame 22 23 0 2
    G.tableAttachDefaults rgtWg canvasTable 0 22 0 2
    swn <- vScrolledWinWithWgtList [rgtWg]  SNone
    --    return ()
    ---------------
    pane <- hPanedArea w swn  700
    G.boxPackStart out pane G.PackGrow 0
    labl <- G.labelNew Nothing -- (Just "bottom")
    G.boxPackEnd out labl G.PackNatural 5
    ---
    return out
    where
        getTableForError :: Int -> Bool  -> IO G.Table
        getTableForError n red = do
            let nn = fromIntegral n
            --
            table <- G.tableNew  n 2 True
            cFrames <- sequence $ replicate n G.frameNew
            lframes <- sequence $ replicate n G.frameNew
            canvases <- sequence $ replicate n (getDrawingCanvas Nothing)
            -- colour the caveses and to the frames --
            if red then
                mapM (\i -> G.widgetModifyBg (canvases !! i) G.StateNormal ((mkRed nn) !! i))  [0 .. n -1]
            else
                mapM (\i -> G.widgetModifyBg (canvases !! i) G.StateNormal ((mkBlue nn) !! i )) [0 .. n -1]
            mapM (\i -> G.containerAdd (cFrames !! i) (canvases !! i)) [0 .. n - 1]
            --- add probabilities to the lables --
            labels <-  mapM (G.labelNew . Just) $ setProbs red (fromIntegral n) (rewriteEfloat 4 . show)
            mapM (\i -> G.containerAdd (lframes !! i) (labels !! i)) [0 .. n - 1]
            -- attatch the frames to the table
            mapM_ (\(bu, x) -> G.tableAttachDefaults table bu 0 1 x (x+1)) $ zip cFrames [0..]
            mapM_ (\(bu, x) -> G.tableAttachDefaults table bu 1 2 x (x+1)) $ zip lframes [0..]
            return table
            where
                --mkRed :: (Fractional a) => a  -> [G.Color ]
                mkRed n =  map mkColor reds -- [ ks | (ks,i) <- zip [(255,  228 - (12 * i) , 247 - (13 * i) )  | i <- [0 .. 2 * n-1]] [1 ..], odd i]
                mkBlue n  =  map mkColor blues -- [ ks | (ks, i) <- zip [ (228 - (12  * i) , 247 - (13 * i) , 255)  | i <- [0 .. 2 * n-1]] [1 .. ], odd i]
                mkColor (a,b,c) = G.Color (257 * a) (257 * b)  (257 * c)
                --
                setProbs :: Bool -> Double -> (Double -> String) -> [String]
                setProbs  b m f
                    | b         =    reverse $ zipWith mkRange (0 : vals) vals
                    | otherwise = zipWith mkRange (0 : vals) vals
                    where
                        vals = map (/m) [1 .. m]
                        mkRange a b = f a ++"-" ++ f b


-- makes a label in a frame
frameLabel :: String -> IO G.Frame
frameLabel  str = do
    label <- G.labelNew $ Just str
    frame <- G.frameNew
    G.containerAdd frame label
    return frame

