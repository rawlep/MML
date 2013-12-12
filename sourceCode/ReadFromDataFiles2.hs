-- ReadFromDataFiles2
{-# LANGUAGE BangPatterns #-}
-- {-# RULES "map/map" forall f g. map f . map g = map (f . g) #-}
-----------------------------------------------------------------------------
-- Module      :  Processfile.ReadFromDataFiles
-- Copyright   :  (c) The University of Nottigham, 2012-2013
-- License     :  see base/LICENSE
-- Maintainer  :  ezzrcp@nottingham.ac.uk
-- Stability   :  internal
-- Portability :  non-portable
-- Handle operations reading and writing to files in the PMS system
-----------------------------------------------------------------------------
module ReadFromDataFiles2 ( INType (Random , GMMLRE)
       , write2log -- ,readRawOutput  --,bs2HMDIF2bs,readFromIntParamsFile, RangeYears,RangeDist,
       ,FileType (SCANNER , SCRIM)
       ,InputPrms (InputPrms, searchDepth,alignMax,alignMin,minTrend ) -- yrsRange,distRange,negSlope,
       ,AdvOptions (SearchDepth,AlignMax,AlignMin,MinTrend)
       ,AdvParmTypes (TInt , TDouble , TBool)
       ----------------
       ,getInputParmOptions,inputPrms2Strings,writeInputPrms,getInputPrms
       ,updateSDepth  --  updateYrsRange,updateDistRange,updateNegSlope
       ,updateAlignMax,updateMinTrend ,updateAlignMin, swap, parseFile,log2File
       ,results2BS'
       --,readTest
       --,testRead -- temporary
       --,testPath -- temporary
       ,bs2HMDIF2bs_aux_cw
       ,bs2HMDIF2bs_aux) where

------
import Time
import Data.Time.Clock
import Data.Time.Calendar
import System.Directory (doesFileExist, doesDirectoryExist,createDirectory,removeFile)
import Data.IORef
import System.IO.Unsafe
--------

--
import qualified Data.ByteString.Lazy.Char8 as L --
import qualified Graphics.UI.Gtk as G {-- (Window, widgetDestroy,textBufferInsertAtCursor,
                                       labelNew,labelSetText,TextBuffer,textBufferDelete
                                       ,textBufferGetEndIter,textBufferGetStartIter,postGUIAsync)
                                       -- ReadFromDataFiles2.hs --}
import Data.List (sortBy,groupBy,partition,unzip3, find, delete,foldl',nub,
                  nubBy,deleteBy, minimumBy, zipWith6)
import Data.Ord (comparing)
import Data.Char (isDigit, isAlpha,isSpace)
import Data.Maybe (listToMaybe, fromJust, isJust)
import Control.Monad (liftM, liftM2)
import Control.Concurrent (forkIO)
import Control.Exception as E (catch,IOException)
import System.Random (newStdGen)
import Control.Parallel.Strategies (using, parList, rpar, rseq)--  as St-- (parMap)
import GlobalParameters (runMsgDialog, DialogType (..) , MyDialog (..), myDialogNew, myDia)
--
import RprMix2 (OModel ,printElm )
import RprGD2 (mml2DsGen,OptimisationType(..))
--


--
---import GlobalParameters (runMsgDialog,Error)
import LMisc (zipMaybe,myTake)
import ProcessFile (Alignment (..),  evl,writeOrAppend,getSections,log2File,
                    seqMap, parMapM,parMapM_ , mapP,bs2Double',intercalate,
                    schemeRec2String,schemeRec2Name, myTime)
import HMDIFPrelude
import Parallel (forkProcessWithScroll,processWithScroll)
import ResultTypes (readSection,sCondition,writeSecOrSchm,readSectionsFromOutFile
    ,takeNChains,section2String,TdsParms(TdsParms),SchemeRec(..),Chain (..), Section (..))
import FileIO (checkIfExtracted,removeExtracted,updateExtracted)
----------------------------------------------------------------------------------------------------

maybe2 :: Maybe a -> Maybe b -> Maybe (a,b)
maybe2 =  liftM2 (,)

{--
seqMap f xs =  (map f xs) `using` evalList rseq
parMapM f = sequence . seqMap f
mapP = parMap rseq
--}

-- Some types symonyms
type Years = [Int]
--type DataValues = [Double]
type BSDefect = L.ByteString
type Date = L.ByteString
type Val = Double
type SectRef = L.ByteString
type SectNode = L.ByteString
type CarriageWay = L.ByteString
type SChain = Double
type EChain = Double

---
bStrings_to_segData :: Bool -> Double ->  Alignment -> -- the alignment to use
    --CarriageWay ->
    [L.ByteString] ->  -- a byteString inpu
    [(SectRef,BSDefect,CarriageWay,[(SChain, EChain, Val)]) ]
bStrings_to_segData isScrim aVal aln  = groupVals .  map (allcT . L.filter (/= ';') )
    where
        --llength  =
        nc2Num n = maybe 0 id . bs2Double1 . nthComma n
        drop2 xs =  (head xs) : (tail $ tail xs)
        reFit  xs   = L.append front (L.intercalate (L.pack ",") . drop2 $ L.split ',' back)
            --L.concat . drop2 . L.split ',' . snd . splitAtChunkBSx (L.pack "SFC")
            where (front,back) = splitAtChunkBSx (L.pack "SFC") xs
        remBlk   =  removeBlock (L.pack "UKMAN,")  -- . reFit -- replaceBS
        allcT bs =
            let (sect, obs, ovl) = findBetweenBSx observDelimiter obValDelimiter bs in
                if isScrim then
                    (remBlk sect, nthComma' 1 obs, nthComma 3 obs, nc2Num 4 obs, nc2Num 5 obs, nc2Num 3 ovl)
                else
                    (sect, nthComma' 1 obs, nthComma 2 obs, nc2Num 3 obs, nc2Num 4 obs, nc2Num 3 ovl)
        --
        last3 (_,_,_,d,e,f)   =  (d,e,f)
        f1 (a,_,_,_,_,_)      = a
        f2 (_,b,_,_,_,_)      = b
        f3 (_,_,c,_,_,_)      = c
        ----
        groupVals    []       = []
        groupVals  (x:xs)     =  (f1 x, f2 x , f3 x , alignAftmap (x:front)) : groupVals back
            where
                (front, back) = partition (eqDeTyp x) xs -- partition
                eqDeTyp (a,b,c,_,_,_) (a1,b1,c1,_,_,_) =  a == a1 && c == c1 -- && b == b1  && d == d1 --   && e == e1
                alignAftmap   =  alignDataRec' 0 aVal aln  .  map last3 --


-- getDetails . bStrings_to_segData
getDetails :: [(SectRef,BSDefect,CarriageWay,[(SChain, EChain,Val)])] -> L.ByteString
getDetails   = L.concat . map flatten
   where
    joinValues (a,b,c) = L.intercalate comma $ map num2BS [a,b,c]
    dataAndvals f      = L.concat . map (f . (<> comma) . joinValues)
    flatten (a,b,c,xs) =
        let appendSect (a, b , c) = L.intercalate comma [a,b,c] in
              dataAndvals (L.append $ L.cons '\n' ((appendSect (a,b,c)) <> comma)) xs
            -- L.cons '\n' (L.intercalate comma [a, b , c ,dataAndvals xs])

--prtYD :: [(SectRef,BSDefect,CarriageWay,[(SChain, EChain,Val)])] -> S
----------
chainSeperator = L.pack "\n-----"
sectionResult = L.pack "\n--Section--------------------------------------------------\n"
---------
getDetailsYears :: [[((SectRef,BSDefect,CarriageWay,SChain, EChain), Val) ]] -> L.ByteString
getDetailsYears  = L.concat . map  (sepFun .  mkDetail . map  joinValues)
   where
    --a separator for each chain
    sepFun     = (chainSeperator <>)
    --
    joinValues ((a,b,c,sc,ec),v) =
        (L.cons '\n' (commas [nC 1 a,nC 2 a ,nC 3 a, b , c,bsc, bec]), commas [nC 4 a, nC 5 a, bv])
        where
            commas = L.intercalate comma
            [bsc, bec, bv] = map num2BS [sc,ec,v]
            nC n        = nthComma n
    -- allocate data to sections
    mkDetail xs | null hd   = L.empty
                | otherwise =  (head hd) <> (L.cons '\n'(L.intercalate eon details))
        where
            (hd, details) = unzip xs
            eon           = L.pack "\n"

filterScheme :: SchemeRec ->  [(SectRef,BSDefect,CarriageWay,[(SChain, EChain, Val)])] ->
                [(SectRef,BSDefect,CarriageWay,[(SChain, EChain, Val)])]
filterScheme schm  = map eqStEnd . filter eqSchm
    where
        sectionRef = scmScannerCode schm
        start      =  fst $ startChain schm
        end        =  snd $ endChain schm
        sorrog     = scmScannerSorrg schm
        eqSchm (st,_,_,xs) = nthComma 2 st == L.pack sorrog && nthComma' 3 st == L.pack sectionRef
        fI         = fromIntegral . floor
        eqStEnd (sc,a,b,xs) = (sc,a,b,ys)
            where
                ys = [ (sc,ec,v) | (sc,ec,v) <- xs , sc >= fI start, ec <= fI end ]

--SECTION\40070,529207007,1080,14112005,14112005,,;

-- extract the file to anlanyse and run the analaysos
gropupByYears :: [(SectRef,BSDefect,CarriageWay,[(SChain, EChain, Val)])] ->
                 -- L.ByteString
                 ([[((SectRef,BSDefect,CarriageWay,SChain, EChain), Val)  ]],[[Val]])
gropupByYears xs = (gbYrs xs , (mapP (snd . unzip) . gbYrs ) xs)
    where
        gbYrs    = nub . groupByYears . flattenWithAlign
        --
        flattenWithAlign xs =
            concat [ mys | (sr,df,cr,ys) <- xs,
                     -- let mSd sc v = (sdate sc , v) ,
                     let mys = map (\(a,b,v) -> ((sr,df,cr,a,b), v ) ) ys ]
        --
        --gropuBySection ::
        --
        groupByYears []       = []
        groupByYears (x:xs)   =
            let (front, back) = partition (equality x) xs in
                (sortBy (comparing date) (x:front)) : groupByYears back
        --
        sdate = nthComma' 4
        date ((st,_,_,_,_),_) =  (L.drop 4 . nthComma' 4) st
        --mDate ((sc,Defect,CarriageWay,SChain, EChain), (Date, Val) )
        equality ((st,df,cw,sc,ec),_) ((st1,df1,cw1,sc1,ec1),_) =
            cw == cw1 && df == df1 && sc == sc1 && ec == ec1 &&
            nthComma' 1 st == nthComma' 1 st1  &&  nthComma 2 st == nthComma 2 st1 &&
            nthComma' 3 st == nthComma' 3 st1

----------------------------------------------------------------------------------------------
--   read data from ./Licenses/parameter.int
----------------------------------------------------------------------------------------------
-- datatype to return
data INType = Random | GMMLRE deriving Show

bs2INType :: L.ByteString -> Maybe INType
bs2INType bs | bs == L.pack "GMMLRE" = Just GMMLRE
             | bs == L.pack "Random" = Just Random
             | otherwise             = Nothing


----------------------------------------------------------------------------------------------------
-- Input Parameters
data AdvOptions = SearchDepth  | AlignMax | AlignMin | MinTrend
--
data InputPrms = InputPrms{
    searchDepth :: Int,     -- search depth
    alignMax    :: Int,     -- maximum number of chains to align
    alignMin    :: Int,     -- minimum number of chains to align
    minTrend    :: Int      -- minimum number of points to trend
    -- min
    }
data AdvParmTypes = TInt Int | TDouble Double | TBool Bool
instance Show AdvParmTypes where
    show (TInt    a)  = show a
    show (TDouble a) = show a
    show (TBool   b) = show b

--- convenience function for reasoning about input parameter types
getInputParmOptions :: InputPrms -> [(AdvParmTypes ,AdvOptions) ]
getInputParmOptions ip =
    [(TInt $ searchDepth ip ,SearchDepth ),
     (TInt $ alignMax ip ,AlignMax),
     (TInt $ alignMin ip ,AlignMin),
     (TInt $ minTrend ip ,MinTrend)
    ]

---
inputPrms2Strings :: InputPrms -> [String]
inputPrms2Strings =  map (show . fst) . getInputParmOptions

-- writing the parameters
writeInputPrms :: InputPrms -> IO ()
writeInputPrms inP = do
    let ldr = "./advps/"
    -- check is the directory exists
    let creatIfnonExist dir yn = if yn then return () else createDirectory dir
    doesDirectoryExist ldr >>= creatIfnonExist ldr
    let fileName = ldr++"advParms.prs"
    -- write to the file
    let mkBs chr =  L.cons chr L.empty
    --
    let sd = toPrint $ searchDepth inP
    let alx = toPrint $ alignMax inP
    let alm = toPrint $ alignMin  inP
    let mT  = toPrint $ minTrend inP
    let vals = [sd, alx,alm,mT] -- yr, dr,ns,
    L.writeFile fileName $ L.intercalate (mkBs ';') vals
    where
        toPrint :: Show a => a -> L.ByteString
        toPrint  =  L.pack . show

-- reads the advanced parameters. Returns Nothing if
-- the file does not exist or the contents are faulty
getInputPrms :: IO (Either String InputPrms)
getInputPrms = do
    let fileName = "./advps/advParms.prs"
    yn <- doesFileExist fileName
    if yn then
        L.readFile fileName >>= return . getVals
    else
        return (Left "The parameter file does not exist. ")
    where
        getVals bs | null lns       = Left "empty parameter file"
                   | null ms        = Left "invalid delimiter in parameter file"
                   | length ms < 4  = Left "insufficient entries in parameter file"
                   | otherwise      = rslt
            where
                lns                 = L.lines bs
                ms                  = L.split ';' $ head lns
                [a,e,f,g]           = myTake 4 ms
                --dbs                 = mapM bs2Double' [b,c]
                its                 = mapM bs2Int' [a,e,f,g]
                rslt                = maybe (Left "invalid numeric value in parameter file")
                                            (\prm -> maybe (Left "intervention valuses not set") Right (iprs prm))
                                            its
                iprs [p,q,r,s]       | any (== 0) [p,q,r,s] = Nothing
                                     | otherwise            = Just $ InputPrms p  q r s



-- updating individual items
updateSDepth :: Int -> InputPrms -> IO ()
updateSDepth n ip = do
    let newIp = ip{searchDepth = n}
    writeInputPrms newIp

--
updateAlignMax :: Int -> InputPrms -> IO ()
updateAlignMax n ip = do
    let newIp = ip{alignMax = n}
    writeInputPrms newIp

--
updateAlignMin :: Int -> InputPrms -> IO ()
updateAlignMin n ip = do
    let newIp = ip{alignMin = n}
    writeInputPrms newIp

--
updateMinTrend :: Int -> InputPrms -> IO()
updateMinTrend n ip = do
    let newIp = ip{minTrend = n}
    writeInputPrms newIp
--
-------------------------------------------------------------------------------------------------
----- the detail returned fomm the network analysis window
data FileType = SCANNER | SCRIM  deriving Show
--
swap :: FileType -> FileType
swap SCANNER = SCRIM
swap SCRIM   = SCANNER
--
--- parsing a HMDIF file
parseFile :: String -> FileType  -> IO (Maybe String)
parseFile filePath ftype = do
    let errString  = "The file at path "++filePath++ " is not a  valid" ++ show ftype
    let errString1 = "file. Please check enter another file and try again"
    -- if parse fails, then return (Just $ errString ++ errString1)
    -- need to completer the parser
    return Nothing

-- -----------------------------------------------------------------------------------------------}
-- Realigning DataBlocks
-- --------------------------------------------------------------------------------------
-- alignDataRec: aligns segment data into chains of len, with each
-- chain starting at 0. Data in overlapping chains according to the
-- specified alidnment parameter
alignDataRec' st end al = alignDataRec st al end
alignDataRec :: Double ->       -- the start value
    Alignment ->                -- the alignment parameter
    Double ->                   -- the end value
    [(SChain, EChain, Val)] ->
    [(SChain, EChain, Val)]
alignDataRec start al len [(sc,ec, val)]
      | ec >   (start + len)  =
        let end = (sc + len) in
            (sc , end , val) : alignDataRec end al len [(end, ec, val) ]
      | otherwise  = [(sc,ec, val)]
alignDataRec start al len ((sc,ec,val):ys@((sc1,ec1,val1):ys'))
    | sc   >= ec    =  alignDataRec start al len ys
    | sc   >= sc1   =  alignDataRec start al len ys
    | sc   == start =  scEqXr
    | sc   <  start =  scLTXr
    | sc   >  start =  scGTXr
    where
        !end     =  start + len
        alN      =  evl al
        front1   = (start, end,  alN val val1)
        rest     = ((end , ec1 , val1) : ys')
        rest1    = ((end , ec1 , alN val val1) : ys')
        scEqXr   | ec == end    = (sc,ec,val) : alignDataRec ec al len ys
                 | ec > end    =
                    if ec < ec1 then
                        (sc, end, val) : alignDataRec end al len rest1
                    else
                        (sc, end, val) : alignDataRec end al len ys'
                 | ec <  end    =
                    if end < ec1 then
                        front1 : alignDataRec end al len rest
                    else
                        front1 : alignDataRec end al len ys'
        --
        scLTXr   | ec  == end = (start, end, val) : alignDataRec end al len ys
                 | ec  > end =
                    if ec1 > ec then
                        (start, end, val) : alignDataRec end al len rest1
                    else
                        (start, end, val) : alignDataRec end al len ys'
                 | ec  <  end =
                    if ec1 > end then
                        front1 : alignDataRec end al len rest1
                    else
                        front1 : alignDataRec end al len ys'
        ----
        scGTXr   | ec == end  = (start, ec , val) : alignDataRec ec al len rest
                 | ec >  end  =
                    if ec < ec1 then
                        (start, end , val) : alignDataRec ec al len rest1
                    else
                        (start, end , val) : alignDataRec ec al len ys'
                 | ec < end   =
                    if end < ec1 then
                        front1 : alignDataRec ec al len rest
                    else
                        front1 : alignDataRec ec al len ys'
--
alignDataRec _ _  _ xs         =   xs
-------------------------------------------------------------------------------------------------

-- flattenHMDIF : Extracts an observation and a carriage way from a HMDIF file. The result of
-- this function is written to a separate file, which is reread in order to do the analysis.
-- This frees up memeory and is more efficient than not writing the result to a file.
-- the file takes : an observation, a carriage way and the input hmdif file and returns
-- a file with only the data relaveant ot that observation and carriage way.
flattenHMDIF :: Bool -> L.ByteString -> L.ByteString -> L.ByteString -> L.ByteString
flattenHMDIF isScrm ob cw =  L.concat . flattendSections .  remTreshld isScrm . findChunkBSx sectionDelimiter
    where
        notDEND = fst . splitAtChunkBS footerDelimiter
        flattendSections :: L.ByteString -> [L.ByteString]
        flattendSections mid  =   map (sectObs . notDEND) sections
            where
                sections   = tail $ splitAtChunksBSx sectionDelimiter  (L.filter (not . isSpace) mid)
                sectObs bs = L.concat $ map ((L.cons '\n' sect) <>)  obsList_aux
                    where
                        (sect, obs) = breakX (== ';') bs
                        obsList_aux = filter selFile $ splitAtChunksBSx observDelimiter obs
                        selFile a = subStringB ob a && subStringB cw a
        -- remove the treshld
        remTreshld :: Bool -> L.ByteString -> L.ByteString
        remTreshld isScrim
            | isScrim   =  L.concat . filter (not . subStringB thresholdDelimiter) . L.lines
            | otherwise = id

------
resultString = L.pack "\n=============== MML2DS Results OMods ===================="
results2BS' :: [([(OModel, ([Double], [Int]))], [Int])] -> L.ByteString
results2BS' results | null results = L.empty
                    | otherwise    = -- (L.cons '\n' resultString) <> -- resultSep <>
                                     (L.concat $ map printResults results) -- <>
                                     -- pGroupsEnd
    where
        printResults (group, cuts) = resultSep  <>
            (L.concat $ map tdsResults group) <> (L.pack ("\ntdsCuts: " ++ printElm cuts))
        resultSep    = L.pack "\n----------------Progression Group --------------------------"
        tdsResults (om,(xs,ys)) = L.concat [L.pack $ show om,printTrRef xs ys] --L.pack $ show xs,newLine
        printTrRef xs ys =
            (L.pack ("\nRpRpredictions: "++ printElm xs ++ "\nRprCutsFreq: "++ printElm ys)) -- s<> (L.pack "\n----")
        --printCuts cts  = pGroupsEnd <> (L.pack ("\ncuts: "++ printElm cts))
-- -}

--- check if analysys was done
--- need a to implement more clever checks
analysisDone :: L.ByteString -> Bool
analysisDone  =  subStringB resultString

--
analysisDone' :: FilePath -> IO Bool
analysisDone'  fileName = doesFileExist fileName

--- testing : reading (writing) from (to) a hmdif file.
bs2HMDIF2bs1 :: String -> String -> FilePath -> IO String
bs2HMDIF2bs1 cw defectName fileName = do
    let defectDir = "./Defects/"
    let outfile = defectDir++defectName++cw++".txt" -- ++ fileName
    (year,month,day) <- getCurrentTime >>= return . toGregorian . utctDay
    let today = L.pack (show month ++ "/" ++ show day++"/"++show year)
    let cway = L.pack cw
    let isScrim = maybe False (== SFC) (string2SCODE defectName)
    let write2outfile = L.readFile fileName >>= \b -> do
        -- create a directory called Defects if it does not exist
        exists <- doesDirectoryExist defectDir
        if exists then
           L.writeFile outfile $ (today <> (flattenHMDIF isScrim (L.pack defectName) cway b)) --
        else do
           createDirectory defectDir
           L.writeFile outfile $ (today <> (flattenHMDIF isScrim (L.pack defectName) cway b))
    write2outfile >> return ("wrote to file: " ++ outfile) -- ++ L.unpack today)


--
bs2HMDIF2bs_aux :: Maybe (String, String) -> --- section and sorrogate
        FilePath ->  -- the filepath with the HMDif file
        [(String -> IO ())] -> -- function to write to a progress dialog
        Maybe MyDialog -> -- SchemeRec -> -- determine whether of not u are analysing schemes
        Maybe Bool -> -- restriction
        (String,Alignment) ->
        (Double, Double) ->  -- years and distance
        Maybe String ->  -- whether or not we do both carriage ways
        IO ()
bs2HMDIF2bs_aux  mSSorg filePath w2buffs mScheme restR (defect, aln) yAndDist mcw  = do
    let extractCarriageWay = bs2HMDIF2bs_aux_cw mSSorg filePath w2buffs yAndDist mScheme  restR (defect, aln)
    E.catch (maybe (mapM_ extractCarriageWay ["CR1","CL1"]) extractCarriageWay mcw)
            (\e -> write2log $ show (e :: E.IOException ))
    return ()
--}


--testRead = bs2HMDIF2bs_aux_cw "simdata01.hmd" print (WithDepth 10) (5 , 35) Nothing Nothing ("LLTX",Max) "CR1"
-- testing the printing of sections
testSectPrint = L.writeFile "./sectTest.txt" . L.pack . section2String

-- the extraction and writing file
bs2HMDIF2bs_aux_cw :: Maybe (String, String) ->
        FilePath ->  -- the filepath with the HMDif file
        [(String -> IO ())] -> -- function to write to a progress dialog (i.e. above the progress bar)
        (Double, Double)  -> --- years and distince
        Maybe MyDialog ->
        Maybe Bool     ->  -- restriction
        (String,Alignment) ->  String -> IO ()
bs2HMDIF2bs_aux_cw mByffs filePath writers yDist mdia restr (defect, aln) cw  = do -- partChn
    -- need an option her to say where the file is to be written
    let isScrim = maybe False (== SFC) (string2SCODE defect)
    let defectFile = "./Defects/" ++ defect ++ cw ++ ".txt"
    --
    scrollLog ("Extracting data for defect: "++ defect ++ ", " ++ cw)
    -- processLog ("Extracting data for defect: "++ defect ++ ", " ++ cw)
    !io <- bs2HMDIF2bs1 cw defect filePath -- >>= \_ -> return () -- logStep
    logLog ("Data for defect: "++ defect ++ ", " ++ cw ++ " extracted Ok..")
    testOpen isScrim restr (defect, defectFile)
    where
        scrollLog        =   (writers !! 0) -- maybe (\_ -> return ())  id (listToMaybe fwrites)
        logLog           =   (writers !! 1) -- maybe (\_ -> return ())  (\_ -> fwrites !! 1) (listToMaybe fwrites)
        processLog       =   (writers !! 2) -- maybe (\_ -> return ())  (\_ -> fwrites !! 2) (listToMaybe fwrites)
        --- combining
        logSLog str      = do {logLog str ; scrollLog str} -- logLog str >> scrollLog str
        processSLog str  = do {processLog str ; scrollLog str}

        -- grouping sections
        groupBySection :: Bool -> [L.ByteString] -> [[L.ByteString]]
        groupBySection  _  []   = []
        groupBySection isScm (x:xs) = ((x:front) : groupBySection isScm back) `using` (parList rseq)
            where
                (front, back) = partition (sectEq x) xs
                [n1,n2,n3]    = if isScm then [2,3,4] else  [1,2,3]
                sectEq ss1  ss2 =
                        let [s1, s2] = map (fst . L.break (==';')) [ss1 , ss2]  in
                            nthComma n1 s1 == nthComma n1 s2 &&
                            nthComma n2 s1 == nthComma n2 s2 &&
                            nthComma n3 s1 == nthComma n3 s2

        --------------------------------------------------------------------------------------}
        --- outputting error messages
        undateErrorMessage ::  String -> Maybe MyDialog -> IO ()
        undateErrorMessage string _ = do
            L.writeFile "./executionError.txt" (L.pack string)
        ---
        -- running the analysis
        testOpen isScrim restr (str, fileName)  = do
            --let [scrollLog, logLog, processLog] = fwrites
            let outFileFun      = if isScrim then  (/='S') else (/='L')
            let outfile         =  "./Defects/"++"out"++dropWhile outFileFun fileName
            let expandedString       = maybe "" display (string2SCODE str)
            ---
            processLog "processing"
            let proceed = do
                fileExists <- doesFileExist outfile
                if fileExists then
                    analysisDone' outfile -- rdidn't implement this
                else return True
            shouldProceedWitAnalysis  <- proceed
            if shouldProceedWitAnalysis then do
                --- do a flag to determine whether to proceed with re-alignment (do i need that?)
                -- ******************* read the sectionFile here --------- *********************
                scrollLog  ("Preprocessing data:  preparing data  for analysis ...")
                L.readFile fileName >>= \bs -> do
                    let cway = L.pack cw
                    sections <- (return . nub . groupBySection isScrim .  tail  . L.lines) bs
                    if null (concat sections) then do
                        let message = "No sections found for "++ expandedString ++ ", " ++ cw
                        logLog message
                        undateErrorMessage message mdia
                    else do -- sections --
                        -- the aval and the alignment should depend on the condition and the type of data being analysed
                        -- aval is 3 for SCRIM and  10 for SCRIM, which anl van be max min or avg depending on
                        -- the scanner parameter selected
                        let alnVal =  if isScrim then 3 else 10
                        --write  ("Preprocessing data:  preparing data  for analysis ...")
                        let (aData,vals) = (unzip . mapP (gropupByYears . bStrings_to_segData isScrim alnVal aln)) sections
                        if (null (concatMap concat vals) || null (concatMap concat aData)) then do
                            let message = "No data found for "++ expandedString ++ ", " ++ cw
                            logLog message
                            undateErrorMessage message mdia
                        else do
                            let sDelimiter = L.pack "\n===-----------result section------------====="
                            let prepareAndWrite = L.intercalate sDelimiter . (L.empty :)
                            --writeOrAppend outfile  (prepareAndWrite $ mapP getDetailsYears aData)
                            L.writeFile outfile  (prepareAndWrite $ mapP getDetailsYears aData)
                            ------- get the section that relates to the desired condition
                            let defectType = (string2SCODE defect)
                            let eqDef df = maybe False (== df) defectType
                            --
                            mSects <- (liftM (valsForCondition (string2SCODE defect) ). readSectionsFromOutFile mByffs) outfile
                            case mSects of
                                Nothing       -> do
                                    let message =  "error reading from HMDIF file. "
                                    logLog message
                                    undateErrorMessage  message mdia
                                Just sectionns -> do
                                    -- need to check if this is null
                                    if null sectionns then do
                                        let appPair (st1,st2) = "section: " ++ st1 ++ " sorrogate: " ++ st2
                                        logLog ("No data for section " ++ (maybe "" appPair mByffs) )
                                        -- G.postGUIAsync $ runMsgDialog Nothing  Error
                                    else do
                                        let sections' =   sectionns
                                        inputParms <- getInputPrms
                                        case inputParms of
                                            Right  iprs -> do
                                                g <- newStdGen
                                                let dpt   = searchDepth iprs   -- search depth
                                                let aMax  = alignMax iprs      -- maximum number of chains to align
                                                let aMin  = alignMin iprs      -- minimum number of chains to align
                                                let minTd = minTrend  iprs     -- minimum number of points to trend
                                                --let analyse xzs = aFun dpt aMax aMin minTd xzs >>= \rlt ->
                                                let tdsP yrs dst = TdsParms dpt  dst yrs restr aMax aMin minTd writers
                                                -- writeSecOrSchm (set the correct file, etc)
                                                let tdp   = tdsP (fst yDist) (snd yDist)
                                                let clcThenPrint sc = mml2DsGen g tdp FromData (Left sc) >>= writeSecOrSchm
                                                --
                                                let !computeMML  = mapM_  clcThenPrint sections'
                                                processLog "Analysis started."
                                                (_, timeMessage) <- myTime computeMML
                                                processLog ("Analysis ended after "++ timeMessage ++ "seconds.")
                                                logLog ("Analysis for " ++ expandedString ++ ", " ++ cw ++ " completed ok ..")

                                            Left string  -> do
                                                let message = "error reading parameter file: "++string
                                                logLog message
                                                --processLog message
                                                undateErrorMessage message mdia
            else do
                logLog ("Analysis already done for "++str ++ ", " ++ cw)
        --- get values for condition
        valsForCondition :: Maybe Defect -> Maybe [Section] ->  Maybe [Section]
        valsForCondition mdefect  =
            case mdefect of
                Nothing     -> id
                Just defect ->
                    case defect of
                        RCI     ->  liftM (concatMap selectedDefets . groupBySorrogateCWay)
                        anyElse ->  liftM (sectByDefect defect)
            where
                --eqDef df     =  maybe False (== df) mdefect
                sectByDefect def =   filter ((== def) . sCondition)
                rciDefects   =  [LLRT,LRRT,LV3,LV10,LTRC,LLTX]
                -- calculate RCI for the selevted defects
                selectedDefets :: [Section] -> [Section]
                selectedDefets sects
                    | 6 /= length rciPrms =  []
                    | otherwise           =  [rciCalc (rciPrms !! 0) (rciPrms !! 1) (rciPrms !! 2)
                                                      (rciPrms !! 3) (rciPrms !! 4) (rciPrms !! 5) ]
                    where
                        rciPrms = (map head . filter (not . null)) [sectByDefect def sects  | def <- [LLRT,LRRT,LV3,LV10,LTRC,LLTX] ]
                        -- calculate
                        rciCalc :: Section -> Section -> Section -> Section -> Section -> Section -> Section
                        rciCalc llrt lrrt lv3 lv10 ltrc lltx =
                            Section RCI (sScannerCode lrrt) (sScannerSorrg lrrt) (sCWay lrrt) combineChains ([],[])
                            where
                                -- applying the rci calculation to each chain
                                applyRCIcalc :: Double -> Double -> Double -> Double -> Double -> Double -> Double
                                applyRCIcalc v1 v2 v3 v4 v5 v6 = v1 + v2 + 0.8 * v3 + 0.6 * v4 + 0.6 * v5 + 0.0 * v6
                                -- combining the chains to make an RCI chain
                                combineChains = [Chain (cInterval c1) rciData rciYear [] | c1 <- sChains llrt
                                                    , c2 <- sChains lrrt
                                                    , c3 <- sChains lv3 , c4 <- sChains lv10
                                                    , c5 <- sChains ltrc, c6 <- sChains lltx
                                                    , let allEq xs = if null xs then False else all (== (head xs)) (tail xs)
                                                    , allEq  $ map  cInterval [c1,c2,c3,c4,c5,c6]
                                                    , let rciData = zipWith6  applyRCIcalc (cData c1) (cData c2) (cData c3) (cData c4) (cData c5) (cData c6)
                                                    , let minNBy f xs = if null xs then [] else minimumBy (comparing f) xs
                                                    , let rciYear = minNBy length $ map  cYears [c1,c2,c3,c4,c5,c6]
                                                 ]
                -- groupBySorrogate
                groupBySorrogateCWay ::   [Section] -> [[Section]]
                groupBySorrogateCWay []      =  []
                groupBySorrogateCWay (x:xs)  =  front : groupBySorrogateCWay back
                    where   (front,back) = partition (\y -> sScannerSorrg x == sScannerSorrg y && sCWay x == sCWay y) xs
----------------------------------------------------------------------------------------------------------------
-- writes to the a default log filem
write2log :: String -> IO ()
write2log  = log2File "logfile1"
--
