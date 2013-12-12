{-# LANGUAGE BangPatterns #-}

module Displays (mkDisplayBox,displayTest) where -- , DataType (SCANNER,SCRIM,Simulation,Accident))  where


import Prelude hiding (tan)
import qualified Graphics.UI.Gtk as G
import qualified Data.ByteString.Lazy.Char8 as L (unpack,dropWhile,pack,concat,writeFile,snoc,cons,append,lines, readFile)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM,liftM2)
import Control.Concurrent (forkIO,killThread)
import Control.Exception (SomeException)
import Data.Maybe (isJust,maybe,listToMaybe)
import Data.List (delete, find, findIndex, deleteBy,maximumBy, nub,foldl',zip3)
import Data.Char (isDigit,isAlphaNum, isSpace)
import Data.Ord (comparing)
import System.Directory (doesFileExist) -- , doesDirectoryExist,createDirectory)
import Data.IORef
import System.Random
-------------------------------------------------------------------------------------------------
-- graphing
import Numeric.LinearAlgebra hiding (find)
import Control.Monad.Trans
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Data.Colour (black, opaque)
import Data.Colour.Names
import RprMix2 (Mmodel (MkModel),r_n)
import LMisc (trendLine,predsFromLP,mkIntervals,zipp3)
import ProcessFile (rewriteEfloat)
-------------------------------------------------------------------------------------------------
import Data.Time.Calendar  -- , fromGregorian)
import Data.Time.LocalTime -- (midnight , LocalTime)

--- test displays for the
import ResultTypes
import GlobalParameters hiding (DrawRef (..)
    , DrawRef (..),Results (..),RResults
    ,updateAddChain,mkDrawRef,emptyDrawRwf,updateDrawData,updateCurrChain
    ,updateCaption,updatePtsLabel,updateFunction
    ,removeFunction, removeResults, updateResults)
--
import HMDIFPrelude (conditions,Defect,display)
import DrawingFrame (colours, reds, blues -- gColors,fileBrowser,colors,
       ,mkDnaDisplayFrame-- mkDnaBase,,isValid,mkDnaTable
       ,frameLabel,getDrawingCanvas) -- vals2drwRef,,comboText,mkProgressionGroupList,
-------------------------------------------------------------------------------------------------
--type DrawRef = Either (Either Scheme Section) (Maybe (Chain,ResultRec))
type ChDref = ([Double],[Double],[Double],[Double], String,[Double])
-- remember, need to modify section an schems for Scrim
------------------------------------------------------------------------------------------------
-- types for displaying the diagram in the view window
data DisplayType = DnaDiagram | ProgressionGroups | Errors

------------------------------------------------------------------------------------------------
mMaxMin b xs
    | null xs  = 1
    | b        = maximum xs
    | otherwise  = minimum xs -- then 1 else
------------------------------------------------------------------------------------------------
sumUp :: Num a => [a] -> [a]
sumUp = foldr (\a vs -> a : map (+a) vs) []

-- disaplayin the results
mkDisplayBox :: FilePath -> IO G.VBox
mkDisplayBox resultsFile = do
        others <- liftM (map L.unpack . L.lines) $ L.readFile resultsFile
        --let resultsFile = "./SectionResults/results.txt"
        mainVBox <- G.vBoxNew False 1
        ---condsRef <- newIORef [] -- an IOref to be used by the clearAdd function
        ntbkRef <- newIORef []
        --------------------
        ntbk <- G.notebookNew
        [tophBox,lhBox] <-  sequence . replicate 2 $ G.hBoxNew False 0
        -------------------------------
        avlCombo <- G.comboBoxNewText
        results <- liftM2 (++) (getConditions conditions "CR1") (getConditions conditions "CL1") -- >>=
        mapM_ (G.comboBoxAppendText avlCombo) (nub others) -- (results ++ others)
        mapM_ (\s -> G.widgetSetSizeRequest s 500 (-1)) [avlCombo]
        avBox <- labelLeftofWidget " Available  results: " avlCombo True
        buttons <- mapM G.buttonNewWithLabel [" View Error ", " View Progression Groups ", " View Errors "]
        let [dnaButton, groupsButton, errorsButton] = buttons
        let displayButtons = zip buttons [DnaDiagram , ProgressionGroups , Errors]
        let buttonHandle (butt, dsp) = G.on butt G.buttonActivated $ conditionComboHandle ntbkRef avlCombo dsp ntbk
        mapM_ buttonHandle displayButtons
        ----
        G.boxPackStart tophBox avBox G.PackNatural 2
        mapM_ (\bt -> G.boxPackEnd tophBox bt G.PackNatural 2) (init buttons)
        ----
        G.boxPackStart mainVBox tophBox G.PackNatural 2
        G.hSeparatorNew >>= \sep -> do
            G.boxPackStart mainVBox sep G.PackNatural 1
        G.boxPackStart mainVBox ntbk G.PackGrow 1
        return mainVBox
        -- sectBox <- labelLeftofWidget " Available scheme results: " sectsCombo True
        --else
        where
            getConditions :: [Defect] -> String -> IO [String]
            getConditions (x:xs) cw = do
                let resultFile = "./SectionResults/"++ show x ++cw++".txt"
                fileExists <- doesFileExist resultFile
                if fileExists then
                    (liftM ((display x ++"-"++ cw) :) . getConditions xs) cw
                else
                    getConditions xs cw
            getConditions  _ _ = return []
            ---
            conditionComboHandle ntbkRef avlCombo dsp ntbk = do
                let ff = id --  fmap $  (\(a,b) -> a ++ getCarriageWay b) . span isAlphaNum)
                !mtext <- ff $ G.comboBoxGetActiveText avlCombo --
                case mtext of
                    Just text -> do
                        let resultFile = "./SectionResults/"++ (filter (not . isSpace) text) ++".txt"
                        --rsect <- readSection  resultFile --"./sectTest.txt" --
                        fileExist <- doesFileExist resultFile
                        if fileExist then do
                            -- !rsect <- readResultsPerChain resultFile
                            !rsect <- readSection resultFile
                            case rsect of
                                Just secR -> do
                                    -- display the errors, DNA diagram or Progression Groups ---
                                    case dsp of
                                        DnaDiagram -> do
                                            let (chains, tdsCuttss) = (sChains secR, sResults secR)
                                            !kss <-  mkDNA False (resultsPerChain chains tdsCuttss)  (snd tdsCuttss)
                                            noteBookAddWidget ntbk ("Error for " ++ text) ntbkRef Nothing (\_ -> return ()) kss
                                        ProgressionGroups -> do
                                            -- mkProgressionGps :: [Chain] -> [Double] -> [Int] -> IO (G.Table)
                                            let (chains, tdsCuttss) = (sChains secR, sResults secR)
                                            -- et mtdsCuts = Just . snd $ sResults secR
                                            !kss <-  mkDNA True (resultsPerChain chains tdsCuttss)  (snd tdsCuttss)
                                            noteBookAddWidget ntbk ("Progression Groups for " ++ text) ntbkRef Nothing (\_ -> return ()) kss
                                        Errors            ->
                                            runMsgDialog Nothing "Handle not yet cmpleted"  Error
                                    ----------------
                                Nothing -> do
                                    runMsgDialog  Nothing "Invalid section read for display"  Error
                        else runMsgDialog Nothing  (resultFile ++" does not exist.")  Error
                    Nothing   ->
                        runMsgDialog  Nothing "No file to selected to view"  Error
                where
                    getCarriageWay :: String -> String
                    getCarriageWay  (_:xs)
                        | xs == "CR1" || xs == "CL1"   = xs
                        | otherwise                    = getCarriageWay xs
                    getCarriageWay    xs               = xs
                    ---
                    getMaxYears :: [[Double]] -> [Double]
                    getMaxYears  xs  = [n .. m]
                        where
                            (n,m) = foldl' mmkMx (5000, 0) xs
                            --
                            mmkMx :: (Double,Double) -> [Double] -> (Double,Double)
                            mmkMx (a,b)  xs
                                | null xs    =   (a,b)
                                | otherwise  =  (an, bn)
                                    where
                                        (n,m) = (mMaxMin False xs, mMaxMin True xs)
                                        an   = if n < a then n else a
                                        bn   = if m > b then m else b
                                     -- in



----------------------------------------------------------------------------------------------
mkDNA ::  Bool -> [(Chain,ResultRec)] -> [Int] -> IO G.VBox
mkDNA  prGroups chinsR tdsCutts = do -- chs  rts
    (tab,chn,yrs',cTable) <-   mkDnaBaseChn prGroups chinsR tdsCutts -- (resultsPerChain chs  rts) -- chs  rts
    let chs = map fst chinsR
    --mkDnaBase chains (map mkChainDt chs) years []
    --G.tableAttachDefaults tab tt 1 (yrs' + 1) 0 chn
    scrWin <- G.scrolledWindowNew Nothing Nothing
    G.scrolledWindowSetPlacement scrWin G.CornerTopRight
    G.set scrWin [G.scrolledWindowHscrollbarPolicy  G.:= G.PolicyAutomatic,
                  G.scrolledWindowVscrollbarPolicy  G.:= G.PolicyAutomatic]
    G.scrolledWindowAddWithViewport scrWin tab
    --mkDnaDisplayFrame :: G.WidgetClass w => w -> G.Table -> IO G.VBox
    mkDnaDisplayFrame  prGroups scrWin cTable
-------------------------------------------------------------------------------------------------

--- displaying the section in a DNA diagram
displayResults :: String -> IO ()
displayResults defect = do
    --G.unsafeInitGUIForThreadedRTS
    let resultFile = "./SectionResults/"++defect++".txt"
    fileThere <- doesFileExist resultFile
    if fileThere then do
        --rsect <- readSection  resultFile --"./sectTest.txt" --
        rsect <- readResultsPerChain resultFile -- :: FilePath -> IO (Maybe [(Chain,ResultRec)])
        case rsect of
            Just secR -> do --  (sChains sec) (sResults sec)
                win <- mkDNA False secR [] >>= widgetInWindow
                G.widgetShowAll win
                G.onDestroy win G.mainQuit >> return ()
            Nothing -> do
                --runMsgDialog  "invalid section"  Error
                print "invalid section"
                --G.mainQuit
    else do
        let str = "There are no results for "++ defect
        let str1 = ". No analysis was done, or it is still awaiting the results."
        runMsgDialog  Nothing (str ++ str1)  Error
        --G.mainQuit
    --G.mainGUI
--}

--- display test./SectionResults/LGRDCR1.txt
displayTest = do
    let resultsFile = "./SectionResults/results.txt"
    G.unsafeInitGUIForThreadedRTS
    win <- mkDisplayBox resultsFile >>= widgetInWindow
    G.widgetShowAll win
    G.onDestroy win G.mainQuit >> return ()
    -- displayResults "LT95CR1"
    --G.mainQuit
    G.mainGUI

--- reading a section
readSectionTest = do
    rsect <- readSection "./SectionResults/LGRDCR1.txt"
    case rsect of
        Nothing       -> print "read error"
        Just sections ->
            print (show $ sections)

----------------------------------------------------------------------------------------------
---             DNA stuff -----------------
----------------------------------------------------------------------------------------------}

-- makes a base diagram - a table withe the years as colums and chainage as rows
-- does not include the canvas for the diagram and histogram
mkDnaBaseChn :: Bool -> [(Chain, ResultRec)] -> [Int] -> IO (G.Table, Int, Int, G.Table)
mkDnaBaseChn  prGroup chResults tdsCutts = do -- tdsCutts
    let (chains,results) = unzip chResults
    let chn = length chResults
    let yrs = mMaxMin True $ map (length . cYears . fst) chResults
    let ys  = filter (not . null ) $ map  (cYears.fst) chResults
    ---
    let cpr    =  abr . snd
    let fPrds  =  rprPredictions . snd
    let fData  =  cData . fst
    let probsWithMissing =  zip (map  cpr chResults) $ zip (map fData  chResults) (map fPrds chResults )
    let yps = maybe (0,0) id (findYrsIntv ys)
    let years =  if null chResults then [1] else  maximumBy (comparing length)  $ map ( cYears . fst) chResults
    ---
    table <- G.tableNew (2 + chn) (1 + yrs)  True
    drwRef <- newIORef Nothing
    chnButtons <- mapM (G.buttonNewWithLabel . mkIntv . cInterval) chains
    mapM_ (\(bu, x) -> G.tableAttachDefaults table bu 0 1 x (x+1)) $ zip chnButtons [1 ..]
    -- label the yers
    yrsLabels <- mapM (frameLabel . show ) years -- [fst yps .. snd yps]
    -- button for the years
    yrsButtons <- mapM (G.buttonNewWithLabel . show) years
    ---
    mapM_ (\(bu, x) -> G.tableAttachDefaults table bu  x (x+1)  (chn + 1) (chn + 2)) $ zip yrsLabels [1..]
    mapM_ (\(bu, x) -> G.tableAttachDefaults table bu  x (x+1)  0 1) $ zip yrsButtons [1..]
    let myZip3 (a,(b,c)) = map (\(p,q,r) -> (p,(q,r))) $ zip3 a b c
    --
    if prGroup then do
        gen <- newStdGen
        let groupLoc  = map  (nub . concatMap (rprCuts . snd) . fst) $ mkIntervals chResults tdsCutts
        -- let numCols   = sum [1 + length ks | ks <- groupLoc ]
        let [a,b,c] =  take 3  $ randomRs (0, 10000000) gen :: [Double]

        let colourRef = concat [ ks | (xs, (mms,j)) <- zip groupLoc (mkIntervals probsWithMissing tdsCutts)
                                    ,let ks = if null xs then repeatHead Nothing mms else concat [repeatHead (Just xs) ns  | (ns, _) <- mkIntervals mms [j] ] ] --
        -- let zs = zip groupLoc (sumUp tdsCutts)
        -- spacing
        mapM_ (flip (G.tableSetColSpacing table) 1) [1 .. yrs]
        mapM_ (flip (G.tableSetRowSpacing table) 1) [0 .. chn]
        ((liftM (zip [1,2..]) . mapM (mkCavases (Just (a,b,c)) . myZip3)) colourRef) >>=
            mkDNAfromCanvases  table [1 .. yrs]
    else do
        -- spacing
        mapM_ (flip (G.tableSetColSpacing table) 1) [1 .. yrs]
        mapM_ (flip (G.tableSetRowSpacing table) 1) [0 .. chn]
        ((liftM (zip [1,2..]) . mapM (mkCavases Nothing . myZip3)) probsWithMissing) >>=
            mkDNAfromCanvases  table [1 .. yrs]
    ---- the canvases table shold be generated outside of this function (in mkDnaBase ?) ---
    canvases <- sequence $ replicate 2 (getDrawingCanvas Nothing)
    --- updating on click of button -- map (\a -> (mkIntv . cInterval $ fst a , a)) $
    let onlyChains = map (\a -> (mkIntv $ cInterval a , a)) chains
    -- updating the canves on a click of the chains button
    sequence $ zipWith (($!) (updateCanvasOnClick drwRef canvases chResults)) chnButtons onlyChains
    -- updating the canvases on a click of the years button ---
    mapM_ (updateGroupsCanvasOnClick drwRef canvases chResults $ map fromIntegral tdsCutts) (zip yrsButtons [0,1 ..])
    --
    canvasTable <- tableWithWidgetList True canvases True 10
    --
    return (table, chn,yrs,canvasTable) -- chainLabes? drawRefs ? canvases ?
    where
        ---
        repeatHead :: Maybe [Int] -> [([Double], ([Double],[Double]))] -> [([Double], ([Double],[Double]))]
        repeatHead ms xs
            | null xs    = []
            | otherwise  =
                case ms of
                    Nothing -> gettHd xs
                    Just ns  ->  take (length xs) $ repeat (getHeadN (head xs) ns) -- map (flip getHeadN ns) xs --
                where
                    gettHd ks = take (length ks) $ repeat (getHead (head ks))
                    repHead ks = (take (length ks) . repeat . head) ks
                    --
                    getHead :: ([Double], ([Double],[Double])) -> ([Double], ([Double],[Double]))
                    getHead (as ,(ys, zs)) = (repHead as, (repHead ys, repHead zs))
                    --
                    getHeadN :: ([Double], ([Double],[Double])) -> [Int] -> ([Double], ([Double],[Double]))
                    getHeadN (as ,(ys, zs)) n = (repHead1 n as, (repHead1 n ys, repHead1 n zs))
                        where
                            repHead1 ns ks = concatMap (repHead . fst) (mkIntervals ks ns)
                               --  let (a,b) = splitAt n ks in
        ---
        probs = map (/10) [1 .. 10]
        -----------mkRed :: (Fractional a) => a  -> [G.Color ]
        mkRed =  map mkColor (reverse reds)
        mkBlue  =  map mkColor  blues
        mkColor (a,b,c) = G.Color (257 * a) (257 * b)  (257 * c)
        whites  n        =  map mkColor . take n $ repeat (255,255,255)
        ------------------- get the label for chains
        mkIntv (a,b) = show (floor a) ++ " - " ++ show (floor b)
        -- creates a list of canvases from a list of probabilities or error
        mkCavases :: Maybe (Double,Double,Double) ->  [(Double, (Double,Double))] -> IO [G.DrawingArea]
        mkCavases  _ []     = return []
        mkCavases  mss  ((x,(d,p)):xs) = do
            -- add a button press event
            case mss of
                Nothing -> do
                    canvas <- G.drawingAreaNew
                    G.widgetAddEvents canvas [G.ButtonPressMask]
                    --if x == 9999 then
                    --    G.widgetModifyBg canvas G.StateNormal (G.Color 255 255 255)
                    --else do
                    --    --let ys' = map (\x ->  x * 0.07689) [1 .. 13]
                    if d >= p then do
                        let indx =   finIndx x probs
                        G.widgetModifyBg canvas G.StateNormal (mkRed !!  indx)
                        (G.on canvas G.buttonPressEvent $ liftIO (showProbability x indx)) >> return ()
                    else do
                        let indx =   finIndx x probs
                        G.widgetModifyBg canvas G.StateNormal (mkBlue !!  indx)
                        (G.on canvas G.buttonPressEvent $ liftIO (showProbability x indx)) >> return ()
                    ys <- mkCavases mss xs
                    return (canvas : ys)
                -- G.on window G.mapEvent $ liftIO removeExtracted -- remove the
                Just ((idx, pp , qp)) -> do
                    let [idx1, p1 , q1] =  map fromIntegral [floor (idx * x) , floor (idx * d) , floor (idx * p) ]
                    cvs <- G.drawingAreaNew
                    G.widgetModifyBg cvs G.StateNormal  (G.Color idx1  p1  q1) -- (colors !! idx)
                    ys <- mkCavases mss xs
                    return (cvs : ys)
        -------------------------------------------------------------
        showProbability :: Double -> Int -> IO Bool
        showProbability  prb value = do
            let rround   = rewriteEfloat 5 . show
            let valString = "Error likelihood: "++ (setVals  value)
            let probString = "Probability of error:  " ++ (rround prb)
            let missingString = "This is a missing value"
            --if prb == missingVal then do
            --    runMsgDialog Nothing missingString Info
            --    return True
            --else do
            runMsgDialog Nothing (valString ++ "\n" ++ probString) Info
            return True
            where
                setVals  i
                    | i <= 1  =   "very low"
                    | i <= 3  =   "low"
                    | i <= 5  =   "marginal"
                    | i <= 7  =   "high"
                    | otherwise = "very high"
                    -- else if  >
        -------------------------------------------------------------
        finIndx :: Double -> [Double] -> Int
        finIndx  x  ys =  findCloseIndx x ys  0
            where
                findCloseIndx :: Double -> [Double] -> Int -> Int
                findCloseIndx y (x:xs) k
                    | y <= x             =  k
                    | otherwise          =  findCloseIndx y xs (k+1)
                findCloseIndx _   _   _  =  0
        -------------------------------------------------------------
        --- attaching the DNA canvases to the DNA table
        mkDNAfromCanvases :: G.WidgetClass w =>  G.Table -> [Int] -> [(Int,[w])] ->  IO ()
        mkDNAfromCanvases   _     _       []      = return ()
        mkDNAfromCanvases  table rows ((i,ws):xs) = do
             mapM_ (\(bu,x) -> do
                       -- print x
                       G.tableAttachDefaults table bu  x (x+1) i (i + 1)) $ zip ws rows
             mkDNAfromCanvases table rows xs
        --updateCanvasOnClick :: IORef (Maybe (Chain,ResultRec)) ->
        --                       [G.DrawingArea] ->
        --                       (G.Button,(Chain,ResultRec)) -> IO ()
        updateCanvasOnClick  dref  canvases rst button (str,chain)  = do
            let (funCanvas, hisCanvas) = (canvases !! 0, canvases !! 1)
            -- have results
            if (not . null) rst then do
                -- print (show rst)
                G.on funCanvas G.exposeEvent $ liftIO $ plotChainFunction funCanvas dref
                G.on hisCanvas G.exposeEvent $ liftIO $ plotChainHistogram hisCanvas dref
                G.on button G.buttonActivated $ do
                    case find ((== chain) . fst) rst of
                        Nothing -> return ()
                        Just (chain, rrec) -> do
                            modifyIORef dref (\_ -> Just (Left (chain, rrec)))
                            --print (show $ rrec)
                            mapM_ G.widgetQueueDraw canvases
            -- no results
            else do
                G.on funCanvas G.exposeEvent $ liftIO $ plotChain funCanvas dref
                G.on button G.buttonActivated $ do
                    let rrec = ResultRec 0 0 0 [1] [1] []
                    modifyIORef dref (\_ -> Just (Left (chain, rrec)))
                    G.widgetQueueDraw funCanvas
        ----
        plotChainFunction :: G.DrawingArea -> IORef (Maybe (Either (Chain,ResultRec) ChDref)) -> IO Bool
        plotChainFunction canvas dref = do
            mdref <- readIORef dref
            case mdref of
                Just (Left (chn, rrec)) -> do
                    -- predsFromLP :: [Double] -> [Int] -> Double -> Double
                    let ds = cData chn
                    let ks =  (rprPredictions rrec)
                    ---- missing value to fit the graph correctly ---
                    let ksd = ds ++ ks
                    let mv = if null ksd then 0 else (mMaxMin False ksd) - 0.5 -- a dummy value to ignore
                    ---------------------------------------------------
                    let ys   = cYears chn
                    let mys  = (head ys) - 1
                    let mxys = (last ys) + 1
                    let ms'  =  (cMissingYrs chn)
                    let ms   =  map date (mys : ms') -- if even (length ys) then   ((mys : ms') ++ [mxys]) else ((mys : ms') )
                    let col = (colours !! 1)
                    -- layout2 xs ys hxs hys col str
                    let (st,ed) = cInterval chn
                    let str =  "Chainage: "++ (show $ floor st) ++ " - " ++ (show $ floor ed)
                    --print (show $ map trd (cYears chn))
                    let renderable = toRenderable $ layout1 ks ds ys ms [mv] col str
                    updateCanvas  renderable canvas
                _    -> return False
        -- plottin the histogram with the errors
        plotChainHistogram :: G.DrawingArea -> IORef (Maybe (Either (Chain,ResultRec) ChDref)) -> IO Bool
        plotChainHistogram canvas dref = do
            mdref <- readIORef dref
            case mdref of
                Just (Left (chn, rrec)) -> do
                    --print (show  (abr rrec))
                    let mkErs x y  =  (x,[ y]) -- if x == missingVal then (x,[]) else
                    let barVals    = zipWith mkErs (cYears chn) ( abr rrec) -- map () $
                    let names      = [] -- map show (cData chn)
                    let renderable = toRenderable $ layoutH barVals names
                    updateCanvas  renderable canvas
                    return True
                _ -> return False
        ---}
        plotChain :: G.DrawingArea -> IORef (Maybe (Either (Chain,ResultRec) ChDref)) -> IO Bool
        plotChain canvas dref = do
            mdref <- readIORef dref
            case mdref of
                Just (Left (chn, rrec)) -> do
                    --print ("addChain is currently: " ++ show (addChain drw))
                    let ds = cData chn
                    let ys = cYears chn
                    --let yH = [(min ds) - 0.2, (max ds) + 0.2]
                    --let ys' = [yH !! 0] ++ ds ++ [yH !! 1]
                    let ms = map date (cMissingYrs chn)
                    let col = (colours !! 1)
                    -- layout2 xs ys hxs hys col str
                    let (st,ed) = cInterval chn
                    let str =  "Chainage: "++ (show $ floor st) ++ " - " ++ (show $ floor ed)
                    let renderable = toRenderable $ layout2 ds ys ms [0] col str
                    updateCanvas  renderable canvas
                    return True
                _    -> return False

        --
        trend_aux  ks pts = plot_lines_style  .> line_color ^= (opaque  green)
                    $ plot_lines_values ^= [zip pts ks] -- mkLineVals f pts
                    $ plot_lines_title ^= "progression rate"
                    $ defaultPlotLines
                    {--
                    price2 = plot_lines_style .> line_color ^= opaque green
           $ plot_lines_values ^= [[ (d, v) | (d,_,v) <- prices, ok (getYear d) ]] --- [[ ((date d m y), v) | (d,m,y,_,v) <- prices, ]]
           $ plot_lines_title ^= "price 2"
           $ defaultPlotLines
                    --}
        --- the error histogram
        error_bars bvals names =
           plot_bars_titles ^= ["Errors"]
           $ plot_bars_spacing ^= BarsFixWidth 20 --  .> line_color ^= opaque blue
           $ plot_bars_titles ^=  names
           $ plot_bars_values ^=  bvals -- [(x, [y])]
           $ defaultPlotBars -- defaultPieLayout
        --}
        --plotting points
        chain_points xsys col = plot_points_style ^= filledCircles 2 (opaque col)
                    $ plot_points_values ^= [ (date x, y) | (x,y) <- xsys] -- zip ys xs
                    $ plot_points_title ^= "points"
                    $ defaultPlotPoints
        -- plotting the error histogram
        layoutH bvals nms = layout1_title ^= "Errors"
                   -- $ layout1_bottom_axis ^: laxis_generate ^= autoIndexAxis (map (show . fst) bvals )
                    $ layout1_plots ^=  [Left (plotBars  $ error_bars bvals nms ) ]
                    $ defaultLayout1
        -- plot data points only
        layout1 ks xs ys hxs hys col str = layout1_title ^= str
                    $ layout1_left_axis ^: laxis_override ^= axisGridHide
                    $ layout1_right_axis ^: laxis_override ^= axisGridHide
                    $ layout1_bottom_axis ^: laxis_override ^= axisGridHide
                    $ layout1_plots ^=   [Left (toPlot  $ chain_points  below blue ),
                                          Left (toPlot  $ chain_points  above red ),
                                          Left (toPlot $ PlotHidden hxs hys),
                                          Left (toPlot  $ chain_points  (zip ys ks) green ) ,
                                          Left (toPlot  $ trend_aux ks $ map date ys)
                                          ]
                    $ layout1_grid_last ^= False
                    $ defaultLayout1
            where
                (below, above) = sepPoints  ks  (zip ys xs)
        ----- plot data points and predictions
        layout2 xs ys hxs hys col str = layout1_title ^= str
                    $ layout1_plots ^=  [Left (toPlot  $ chain_points (zip ys xs) col ),
                                         Left (toPlot $ PlotHidden hxs hys )]
                    $ defaultLayout1
            where
                xHide = [head xs - 1 , last xs + 1]
                yHide = [head ys - 0.2, last ys + 0.2]
        -----
        mkLtimePlt ys ks =  [ (mkDate 01 01 yyyy, k) |  (yyyy,k) <- zip ys ks]
        -----
        mkLineVals :: (Double -> Double) -> [Double] -> [[(Double,Double)]]
        mkLineVals f  xs@(a:b:ks) = [map (\i -> (i,f i)) xs]
        mkLineVals _      _       = []

        --- sepearating data points below and above the line
        sepPoints :: [Double] -> [(Double,Double)] -> ([(Double,Double)],[(Double,Double)])
        sepPoints       (x:xs)   ((a,ds):ys) =
            let (below, above) = sepPoints xs ys in
                if ds < x then ((a,ds) : below, above) else (below, (a,ds) : above)
        sepPoints     _             _        = ([],[])
        ---
        -- date :: Int -> Int -> Double -> Loca
        date yyyy = (LocalTime (fromGregorian (fromIntegral $ floor yyyy) 1 1) midnight)
------------------------------------------------------------------------------------------
--- show the progression groups
------------------------------------------------------------------------------------------
mkProgressionGps :: [(Chain,ResultRec)] -> [Double] -> [Int] -> IO G.ScrolledWindow -- IO (G.Table)
mkProgressionGps  chains years tdsCts = do
    let chn = length chains
    let yrs = length years---
    let yps = if null years then  (0,0) else (head years, last years)


    table <- G.tableNew (1 + chn) (1 + yrs)  True
    -- the table for the longlitudinal cuts
    -- groupsTable <- G.tableNew (length tdsCts) 1  False
    ---
    let mkIntv (a,b) = show (floor a) ++ " - " ++ show (floor b)
    chnLbls <- mapM (frameLabel . mkIntv . cInterval. fst) chains
    mapM_ (\(bu, x) -> G.tableAttachDefaults table bu 0 1 x (x+1)) $ zip chnLbls [0..]
    -- add the years
    yrsLabels <- mapM (frameLabel . show . floor) years -- [fst yps .. snd yps]
    mapM_ (\(bu, x) -> G.tableAttachDefaults table bu  x (x+1) chn (chn + 1)) $ zip yrsLabels [1..]
    let mkGroupTables n ww = tableWithWidgetList True ww True n
    let groupLoc  = map  (nub . concatMap (rprCuts . snd) . fst) $ mkIntervals chains tdsCts
    -- let sumUp = foldr (\a vs -> a : map (+a) vs) []
    let tss   = sumUp tdsCts
    mapM_ (\(bu, (l,r)) -> do
                --print ("l is: " ++ show l ++", r is: " ++ show r)
                let ks =  if null bu then 1 else 1 + length bu
                let mx = 1 + yrs
                canvases <- sequence $ replicate ks mkColCanvas
                -- print (show $ length canvases)
                let pss = zip (0 : bu) (bu ++ [mx])
                rowtable <- G.tableNew 1 ks  False
                mapM_ (\(c, (ll,rr)) -> do
                           G.tableAttachDefaults rowtable c ll rr  0 1) $ zip canvases pss
                G.tableAttachDefaults table rowtable 1 mx l r) $ zip groupLoc (zip (0 : tss) tss)
    vScrolledWinWithWgtList [table] VScroll
    --}
    --
    where
        mkColCanvas ::  IO G.DrawingArea
        mkColCanvas   = do
            gen <- newStdGen
            let [idx, p , q] = take 3  $ randomRs (0, 10000000) gen :: [Int]
            let [idx1, p1 , q1] =  map fromIntegral [idx, p , q]
            -- print ("index is: " ++ show idx)
            cvs <- G.drawingAreaNew
            G.widgetModifyBg cvs G.StateNormal  (G.Color idx1 p1 q1) -- (colors !! idx)
            return cvs

-------------------------------------------------------------------------------------------
mkDate dd mm yyyy =
    LocalTime (fromGregorian  (fromIntegral yyyy) mm dd) midnight

------------------------------------------------------------------------------------------
-- updateGroupsCanvasOnClick :: [G.DrawingArea] -> [(Chain,ResultRec)] -> [Double] -> IO ()
-- dref  canvases rst button (str,chain)
updateGroupsCanvasOnClick dref [c1, c2] rst cutts (button, i) =  do
    --funCanvas <- getDrawingCanvas Nothing
    G.on c1 G.exposeEvent $ liftIO $ plotGroupsPlots c1 dref -- plotChainFunction funCanvas dref
    G.on c2 G.exposeEvent $ liftIO $ plotGroupsHist c2 dref -- plotChainHistogram hisCanvas dref
    ---  = (canvases !! 0, canvases !! 1)
    G.on button G.buttonActivated $ do
        if (not . null) rst then do
            let chns = [ (show a , show b)  | (ch,_) <- rst , let (a,b)  = cInterval ch]
            let xLabl = maybe "" (\a -> fst (head chns) ++ " to " ++ snd (last chns)) (listToMaybe chns)
            let xVals = [1 .. fromIntegral $ length chns]
            let yVals = [ rss !! i | (c_,rs) <- rst , let rss = rprPredictions rs, length rss > i ]
            let yData = [ css !! i | (ch,_) <- rst , let css = cData ch, length css > i ]
            let errs  = [ ess !! i | (_,rs) <- rst , let ess = abr rs, length ess > i ]
            let drf = (xVals, yVals, yData,errs,xLabl, sumUp cutts)
            modifyIORef dref (\_ -> Just (Right drf))
            --let xxx = "xvals: " ++ show xVals
            --let yyy = "yvals: " ++ show yVals
            -- print (xxx ++"\n" ++ yyy)
            mapM_ G.widgetQueueDraw [c1, c2]
            {-
            funCanvas <- getDrawingCanvas Nothing
            hisCanvas <- getDrawingCanvas Nothing
            --- box with the legend
            let mkTable ws = tableWithWidgetList True ws True 1
            disps <- mapM (G.labelNew. Just) [show i ++ " -- " ++ show a ++ "-"++ show b | ((ch,_),i) <-   zip rst [1,2 ..]
                                                                                      ,let (a,b)  = cInterval ch]
            legendBox <-  mkTable disps
            table <- G.tableNew 1 3 False
            -- tableWithWidgetList :: G.WidgetClass widget => Bool -> [widget] -> Bool -> Int -> IO G.Table
            -- tableWithWidgetList isVertical  widgets homo spacing
            mapM_ (\(bu, x) -> G.tableAttachDefaults table bu  x (x+1) 0 1) $ zip [funCanvas,hisCanvas] [0..]
            G.tableAttachDefaults table legendBox  2 3 0 1
            --- make a window to display them in
            win <- G.windowNew
            -- }
            G.containerAdd win table
            G.on funCanvas G.exposeEvent $ liftIO $ return True -- plotChainFunction funCanvas dref
            G.on hisCanvas G.exposeEvent $ liftIO $ return True -- plotChainHistogram hisCanvas dref
            mapM_ G.widgetQueueDraw [funCanvas,hisCanvas]
            G.widgetShowAll win
            --}
        else
            runMsgDialog Nothing "Nothing to view"  Error
        where
            plotGroupsHist :: G.DrawingArea -> IORef (Maybe (Either (Chain,ResultRec) ChDref)) -> IO Bool
            plotGroupsHist canvas dref = do
                drf <- readIORef dref
                case drf  of
                    Just (Right (xs,_,_,errs, _, _)) -> do
                        let renderable = toRenderable $ layoutH xs errs
                        updateCanvas  renderable canvas
                        return True
                    _    -> return False
            --
            plotGroupsPlots canvas dref = do
                mdref <- readIORef dref
                case mdref of
                    Just (Right (xVals, yVals, yData,_, xLabl, cutts)) -> do
                        let renderable = toRenderable $ layout xVals yVals yData xLabl cutts
                        updateCanvas  renderable canvas
                        return True
                    _    -> return False

        -- drawing
        -- chart1 = layout
        --    where
            plotCuts cs = plot_lines_style .> line_color ^= opaque black
                $ plot_lines_values ^= map (\x -> [ (x , y) | y <- [0 .. 10]]) cs --  , x <- cs ]]
                $ plot_lines_title ^= "Cuts"
                $ defaultPlotLines

            -- line for results
            results xs ys lb = plot_lines_style .> line_color ^= opaque green
                $ plot_lines_values ^= [zip xs ys] --- [[ ((date d m y), v) | (d,m,y,_,v) <- prices, ]]
                $ plot_lines_title ^= lb
                $ defaultPlotLines


            -- plotting points
            origData xs ys = plot_points_style ^= filledCircles 2 (opaque blue)
                    $ plot_points_values ^= zip xs ys -- zip ys xs
                    $ plot_points_title ^= "data"
                    $ defaultPlotPoints

            -- plotting a histoogram
            --- the error histogram
            error_bars bvals names =
                plot_bars_titles ^= ["Probability"]
                $ plot_bars_spacing ^= BarsFixWidth 20 --  .> line_color ^= opaque blue
                $ plot_bars_titles ^=  names
                $ plot_bars_values ^=  [(x,[y]) | (x,y) <- bvals] -- [(x, [y])]
                $ defaultPlotBars -- defaultPieLayout

            -- plotting a histogram
            layoutH ys es  = layout1_title ^= "Errors"
                -- $ layout1_bottom_axis ^: laxis_generate ^= autoIndexAxis (map (show . fst) bvals )
                $ layout1_plots ^=  [Left (plotBars $ error_bars (zip ys es) []) ]
                $ defaultLayout1
            --
            layout xs ys ds xlabl ctts = layout1_title ^= "Results by Years"
                $ layout1_left_axis ^: laxis_override ^= axisGridHide
                $ layout1_right_axis ^: laxis_override ^= axisGridHide
                $ layout1_bottom_axis ^: laxis_override ^= axisGridHide
                $ layout1_plots ^= [Left (toPlot $ results xs ys xlabl), Left (toPlot $ origData xs ds),
                                    Right (toPlot $ plotCuts ctts )]
                $ layout1_grid_last ^= False
                $ defaultLayout1


        ---}
------------------------------------------------------------------------------------------}
--- Canvas and displays
------------------------------------------------------------------------------------------
{- local definitions for drawing with Sections, Schemes, etc
getDrawingCanvas :: Maybe (IORef DrawRef) ->  IO G.DrawingArea
getDrawingCanvas  menbRef = do
    canvas <- G.drawingAreaNew
    G.widgetModifyBg canvas G.StateNormal (G.Color 65535 65535 65535)
    G.widgetAddEvents canvas [G.Button1MotionMask,G.Button1MotionMask,G.ButtonPressMask]
    maybe (return canvas)
          (\ref -> do
                G.on canvas G.exposeEvent $ liftIO $  updateDrawingArea canvas ref
                return canvas
          ) menbRef

--
updateDrawingArea :: G.DrawingArea -> IORef DrawRef -> IO Bool
updateDrawingArea canvas dref  = do
    return True
--}
