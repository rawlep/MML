module ResultTypes (
    ResultRec (..), Results (..) , Chain (..), Section (..)
    ,Scheme (..),SchemeRec (..), TdsParms (..), SecOrSchm
    ,section2String,schemeRec2String, omd2ResultRec
    ,omd2Results,readSection,writeSecOrSchm
    ,readSectionsFromOutFile,resultsPerChain --  tdsChains
    ,readResultsPerChain
    ,takeNChains
    ,readResults
    -- ,missingVal
    --- utilities mainly for creating DNA diagram
    ,fillMissingData,findYrsIntv,findMissing -- fillMissingData,
    ) where

-- this module contains datastructures and functions for storing and
-- writing to/from network and scheme file.


import qualified Data.ByteString.Lazy.Char8 as L
import Numeric.LinearAlgebra (toList,fromList)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (doesFileExist, doesDirectoryExist,createDirectory,removeFile)
import Data.Maybe (isJust,listToMaybe, fromJust)
import Data.Char (isAlphaNum,isDigit,isSpace,isAlpha) -- , isAlpha,,
import Control.Monad (liftM,liftM2)
import Control.Parallel.Strategies (using, parList, rpar, rseq)
import Data.List (sortBy,groupBy,partition,unzip3, intercalate, find,delete,foldl',nub, nubBy,deleteBy,sort)
--
import LMisc (printElm,printElmChr,mkIntervals, myTake, sumBy,zipp, trendLine)
import ListStats (mean, normalP, stDivM,stNorm)
import HMDIFPrelude -- hiding (bs2Double)
import ProcessFile (adjustValues)
import RprMix2 (alph, sigD, sigE, tau, r_n, aBar,sim_eq,OModel (Omod),Mmodel (..))
import LINreg2
----------------------------------------------------------------------------------------------
-- Some utilities
------ find missing values from a list of numbers
findMissing :: (Num a, Enum a) => [a] -> [a]
findMissing (a:b:xs) = (a : [ i | i <- [a .. b], i /= a , i /= b]) ++ findMissing (b:xs)
findMissing    xs    = xs

-- fill in missing data
fillMissingData :: [Double] -> [Double] -> [Double]
fillMissingData datA years = datA -- fillMissingData_aux $ zip datA years
    where
        missingVal  = 9999
        --
        fillMissingData_aux :: [(Double,Double)] -> [Double]
        fillMissingData_aux (a:b:xs)
            | snd b == (1 + snd a) = fst a : fillMissingData_aux (b:xs)
            | otherwise            = fst a : (ks ++ fillMissingData_aux  (b: xs))
            where
                k  = snd a
                k' = snd b
                ks = map (\_ -> missingVal) [ i | i <- [k .. k'], i /= k, i /= k' ]
        fillMissingData_aux xs      = map fst xs
    --where
    --    missingYrs = findMissing years
--}
-- insert missing years and values in order ot make a consecutive list
insertMissing :: [Double] -> [Double] ->  (Double,Double) ->
                 Maybe ([Double] -> [Double]) -> ([Double],[Double])
insertMissing  values years  (mn,mx) =
    maybe (unzip inserted) (\f -> (unzip . insertMissing_aux (zip values years)) (zip (f mmNX) [mn .. mx]) )
    where
        len       = length years
        mmNX      = [1 ..  (1 + mx - mn)]
        inserted  = insertMissing_aux (zip values years) (zip aVals [mn .. mx])
        aVals     = pMissing len values
        --   | otherwise = (n+1) `div` 2
        pMissing :: Int ->  [Double] -> [Double]
        pMissing n vs      = map (ff (lineFun1 $ fromList vs)) mmNX
            where
                lineFun1 ks = let (a,b) = lineFun ks in (abs a, b)
                ff (m,c) x = m * x + c
        --
        insertMissing_aux :: [(Double,Double)] -> [(Double,Double)] -> [(Double,Double)]
        insertMissing_aux ks@((d,y):ys) ((ad,ay):xs)
            | y > ay     =  (ad,ay) : insertMissing_aux ((d,y):ys) xs
            | otherwise  =  (d,y)   : insertMissing_aux  ys xs
                    -- | otherwise  =  (ad,ay) : insertMissing_aux ks xs -- (d,y)   : insertMissing_aux ys xs
        insertMissing_aux _  xs  =  xs

-- calculate up to max years
uptoMaxYrs :: [Double] -> Double -> [Double]
uptoMaxYrs  xs   mx
    | null xs       = xs
    | otherwise     = findMissing (init xs) ++ [last xs .. mx]

--- foldr :: (a -> b -> b) -> b -> [a] -> b
findYrsIntv :: [[Double]] -> Maybe (Double,Double)
findYrsIntv =  foldr finD Nothing
    where
        finD xs mab -- (a,b)
            | null xs   = Nothing -- (0,0)
            | otherwise = maybe (Just (mn, mx)) (\(a,b) -> Just (min a mn, max b mx)) mab
            where
                (mn,mx) = (minimum xs, maximum xs)

----------------------------------------------------------------------------------------------

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

----------------------------------------------------------------------------------------------
data TdsParms = TdsParms {
    searchDept :: Int,    -- search depth
    rngInYrs   :: Double, -- range in years
    rngInDist  :: Double, -- range in distance
    negSlopes  :: Maybe Bool,   -- restrictions for slopes
    maxChn2Aln :: Int,    -- the maximum number of chains to align
    minChn2Aln :: Int,    -- the minimum number of chains to align
    minPts2Trd :: Int,   -- the minimum number of points to trend
    logFun     :: [(String -> IO () )] -- function to log to a file
    }
----------------------------------------------------------------------------------------------
-- Result Attributes
--- ResultRec, Results (..) , Chain (..), Section (..), section2String,Scheme (..)
--- SchemeRec (..), schemeRec2String,
----------------------------------------------------------------------------------------------
-- type TDSCuts = [Int]
type From = Double
type To = Double
type Results = ([ResultRec],[Int])
type SecOrSchm = Either Section Scheme
--
missingVal = 9999.0
----------------------------------------------------------------------------------------------
data ResultRec = ResultRec {
    alp ::  Double,
    std :: Double,
    ste :: Double,
    abr :: [Double],
    rprPredictions :: [Double],
    rprCuts :: [Int]
    } deriving (Eq, Show)

--
extendPredictions :: [Double] -> [Int] -> [Double] -> [Double]
extendPredictions rprPrd cuts = map (trendLine rprPrd cuts)

-- trendLine :: [Double] -> [Int] -> Double -> Double

-- sections
data Chain = Chain {
    cInterval :: (Double, Double),
    cData :: [Double],
    cYears :: [Double],
    cMissingYrs :: [Double]
    } deriving (Show, Eq)

-- sections
data Section = Section {
    sCondition     :: Defect, -- ^ the condition being analysed for the section
    sScannerCode   :: String, -- ^ the scanner section code
    sScannerSorrg  :: String, -- ^ the scanner section sorrogate
    sCWay          :: String , -- ^ the carraige way of the section
    sChains        :: [Chain],
    sResults       :: Results -- ^ the results for the condition
    } deriving Show


--- scheme
data Scheme = Scheme{
    schemeName :: String,       -- ^ the name of the scheme
    schmCondition :: Defect,    -- ^ the conditon to be analysed for the scheme
    schmRecs :: [SchemeRec]    -- ^ the schemRec making up the scheme
    }

--
-- up a scheme.
data SchemeRec = SchemeRec {
    scmScannerCode    :: String,  -- ^ the scanner code for the scheme
    roadClassCode  :: String, -- ^ the road clas name (e.g. A2345)
    roadName       :: String, -- ^ the roa name (empty string if the name UNNAMED)
    scmScannerSorrg   :: String, -- ^ the scannner section sorrogate
    --- carriage way
    startChain   ::  (From, To), -- ^ the start chain (printed as (from - to)
    endChain     ::  (From, To),
    -- input attributes -- (we can calculate the missing data from the missing years)
    scmChains :: [Chain],
    scmResults :: Results -- ^ the results for the scheme
    } deriving Eq
---------------------------------------------------------------------------------------------
-- extracting schemes from sections
type Interval = (Double,Double)

--- we can calculate a schemeRec from a section, given a a few params. This
{---
-- function is for extracting data and shold not be used for extracting
--- results (At let not yet)
section2SchemeRec :: String -> String -> (From, To) -> (From, To) -> Section -> SchemeRec
section2SchemeRec rClass rName  start end sect =
    SchemeRec scode rClass rName ssorg start end schains srsts
    where
        scode   = sScannerCode sect
        ssorg   = sScannerSorrg sect
        cway    = sCWay sect
        schains = [ chn | chn <- sChains sect, let ks = cInterval chn
                          , ks >= start && ks <= end]
        srsts   = sResults sect
--}

section2SchemeRec :: Section ->
   --- String -> -- schemeName
    String -> -- roadnem
    String -> -- roadClass code
    Interval -> -- interval Start
    Interval -> -- chain end
    SchemeRec
section2SchemeRec sect  rdName rdClass start end = -- schmName
    -- Scheme schmName (sCondition sect
    sect2SchmRec sect rdName rdClass start end
    where
        sect2SchmRec :: Section -> String -> String -> Interval -> Interval -> SchemeRec
        sect2SchmRec sct rname rclass st en =
            SchemeRec (sScannerCode sct) rclass rname (sScannerSorrg sct) st en schs (srts,rcts)
            where
                schs = [ chn | chn <- sChains sct, let ci = cInterval chn, st >= ci , ci <= en ]
                srts = [ rt | (chn, rt) <- resultsPerChain (sChains sct)  (sResults sct)
                              ,let ci = cInterval chn, st >= ci , ci <= en ]
                rcts = [ n | (chs, n) <- mkIntervals (sChains sct) (snd $ (sResults sct))
                             , let inRange cs = let ci = cInterval cs in  st >= ci && ci <= en
                             , let hs = filter inRange chs   ]
                -- mkSchmTdsCuts_aux :: Interval -> Interval


---------------------------------------------------------------------------------------------
omd2ResultRec :: (OModel, ([Double], [Int])) -> ResultRec
omd2ResultRec  (Omod m _, (xs,ys)) = ResultRec (alph m) (sigD m) (sigE m) (toList $ aBar m)  xs ys

omd2Results :: ([(OModel, ([Double], [Int]))], [Int]) -> Results
omd2Results  (xs,cts)   =   (map omd2ResultRec xs, cts)
-- tdsChains
----------------------------------------------------------------------------------------------}
--- do I need a footer to indicate the end of these recs?
-- printElm,printElmChr
-- printing results
printResultsRec :: ResultRec -> String
printResultsRec rrec = intercalate "\n" recs
    where
        recs = rec1 ++ rec2 ++ ["rprCuts: " ++ printElm (rprCuts rrec)]
        rec1 = ["alp: "++ show (alp rrec) ,"std: "++ show (std rrec) , "ste: " ++ show (ste rrec)]
        rec2 = ["abr: " ++ printElm (abr rrec), "rprPredictions: " ++ printElm (rprPredictions rrec)]

-- "std: ","ste: ","abr: ","rprPredictions: ","rprCuts: "
-- printing results
results2String :: Results -> String -- intercalate "ResultRec ...\n" .
results2String (a,b) =  (intercalate "\n" . map printResultsRec) a ++
                       ('\n': ("tdsCuts: " ++ printElm b)) ++ "\n endOfResults"

------------------------------------------------------------------------------------------------
-- Scanner Network Level
------------------------------------------------------------------------------------------------


{--- calculate a single chain from a list of chains for mml2ds. Returns a list
--- of the interval bloacks as well
tdsChains :: [Chain] -> ([(Double,Double)] , Chain)
tdsChains chs = (intervals , Chain blockChain cds cys cms)
    where
        intervals          = map cInterval chs
        blockChain         = maybe (0,10) id (maybe2 (isLast ps) (isLast qs))
        (ps,qs)            = unzip intervals
        (cds, cys,cms)     = foldr app3 ([],[],[]) chs
        app3 ch (xs,ys,zs) = ((cData ch) ++ xs, (cYears ch) ++ ys , (cMissingYrs ch) ++ zs)
        isLast  xs         = if null xs then Nothing else (Just $ last xs)


----
data ResultRec = ResultRec {
    alp ::  Double,
    std :: Double,
    ste :: Double,
    abr :: [Double],
    rprPredictions :: [Double],
    rprCuts :: [Int]
    } deriving (Eq, Show)

--
extendPredictions :: [Double] -> [Int] -> [Double] -> [Double]
extendPredictions rprPrd cuts = map (trendLine rprPrd cuts)

--}

---  making strings
chains2String :: Chain -> String
chains2String chn = intercalate "\n"  chainContents ++ "\n EOChn"
    where
        chainContents = ["cInterval: " ++ let (a,b) = cInterval chn in (show a ++ " - " ++ show b) ,
                         "cData: " ++ (printElm $ cData chn),
                         "cYears: " ++ (printElm $ cYears chn),
                         "cMissingYrs: " ++ (printElm $ cMissingYrs chn)
                         ]

-- getting the results for a given section
resultsPerChain :: [Chain] -> Results ->  [(Chain,ResultRec)]
--- Note, when we consider chain results we ignore tds cuts and rpr cuts, and put
--- errors in abar since we do not need these for plotting a function or identifying errors
resultsPerChain chns (results, cuts) = getChnResults chns newResults -- concatMap (uncurry getChnResults) chnRs
    where
        ---
        dNum          = maybe 0 (length . cData) (listToMaybe chns)
        newResults    = concat [ replicate n (rlt {rprPredictions = take dNum prds})  | (rlt, n) <- zip results cuts
                                                                                        ,let prds = rprPredictions rlt  ]
        --groupUp cns   = map  fst . mkIntervals cns
        --chnRs       = zip (groupUp (calcMissing chns) cuts) results
        getChnResults :: [Chain] -> [ResultRec] -> [(Chain,ResultRec)]
        getChnResults chs rrecs =  [setErr c r |
                                        (c,r) <- zip  (calcMissing funList chns) rrecs] -- (map mkCnRslt chnPreds)
            where
                mkTR ch rl  = Just (extendPredictions (rprPredictions rl) (rprCuts rl))
                funList     = sequence $ zipWith mkTR chs rrecs
                --- -- trend the predictions for the mising value --}
                upTo   xs       = [i | (_,i) <- zip xs [1 .. ] ]
                --
                setErr :: Chain -> ResultRec -> (Chain,ResultRec)
                setErr   ch  rrc     = (ch,nrrc)
                    where
                        rrPrd        = (rprPredictions rrc)
                        nprds        = extendPredictions rrPrd (rprCuts rrc) (upTo $ cYears ch)
                        [ap ,st,se] = [(alp rrc) ,  (std rrc) , (ste rrc)]
                        nrrc   = ResultRec ap st se  (calErr (cData ch) nprds)
                                           rrPrd (rprCuts rrc)

                        calErr :: [Double] -> [Double] -> [Double] -- (r_n modl)
                        calErr  ds  ps  =  errors -- map (/sm) errs --
                            where
                                -- predsFromLP :: [Double] -> [Int] -> Double -> Double
                                -- ps    =  nprds -- rprPredictions rs
                                getErr ps ks = r_n modl ps ks
                                modl    = MkModel ap st se (fromList [1 .. 10.0]) 0
                                errors  = zipWith (\a b -> 1 - getErr a b)  ps ds
                                --errs    = zipWith mKE ps ds
                                mKE a b = (1 - getErr a b) * (a - b)**2
                                --sm      = sum errs
                                --
                extendPredictions :: [Double] -> [Int] -> [Double] -> [Double]
                extendPredictions rprPrd cuts = map (trendLine rprPrd cuts)
-------------------------------------------------------------------------------------------------}

--
takeNChains n sec =
    Section (sCondition sec) (sScannerCode sec) (sScannerSorrg sec)
            (sCWay sec) (map nChains $ take n $ sChains sec) (sResults sec)
    where
        -- chains  =  sChains sec
        nChains chn =  Chain (cInterval chn) (myTake m $ cData chn) (myTake m $ cYears chn) (myTake m $ cMissingYrs chn)
        m           =  n + 1

--printing Sections
section2String :: Section -> String
section2String sec = intercalate "\n"  sectContents
    where
        sectContents = [ "sCondition: " ++ (show $ sCondition sec), "sScannerCode: " ++ sScannerCode sec,
                         "sScannerSorrg: " ++ sScannerSorrg sec, -- "sData: " ++ (printElm $ sData sec) ,
                         "sCWay: " ++ sCWay sec,
                         "sChains: " ++ (intercalate "\n" $ map chains2String $ sChains sec) ,
                        -- "sMissingYears: " ++ (printElm $ sMissingYears sec),
                         "sResults: " ++ (results2String $ sResults sec)
                       ]
------------------------------------------------------------------------------------------------
--- Scheme Level -- Note, only one condition is to be analysed per scheme.
------------------------------------------------------------------------------------------------


-- printing a scheme
scheme2String :: Scheme -> String
scheme2String scm = intercalate "\n" scheme
    where
        scheme = ["schemeName: " ++ schemeName scm,
                  "schmConditions: " ++ show (schmCondition scm),
                  "schemRecs: " ++
                   intercalate "schemRecs: \n"  (map schemeRec2String (schmRecs scm)) ]


-- a scheme record contains both raw data and results for section portions that make

---
--schemeRec2String :: SchemeRec -> String
--schemeRec2String src =



-- printing scmemerecs
schemeRec2String :: SchemeRec -> String
schemeRec2String src =  intercalate "\n" scmemrec
    where
        scmemrec = ["scmScannerCode: " ++ scmScannerCode src ,
                    "roadClassCode: " ++ roadClassCode src,
                    "roadName: " ++ roadName src,
                    "scmScannerSorrg: " ++ scmScannerSorrg src,
                    "startChain: " ++ let (a,b) = startChain src in (show a ++ " - " ++ show b) ,
                    "endChain : " ++  let (a,b) = endChain src in (show a ++ " - "  ++ show b)  ,
                    "scmChains: " ++ (intercalate "\n" $ map chains2String $ scmChains src) ,
                    --"scmData: " ++ printElm (scmData src),
                    --"scmYears: " ++ printElm (scmYears src) ,
                    --"scmMissingYears: " ++ printElm (scmMissingYears src) ,
                    "scmResults: " ++  (results2String $ scmResults src)]

---- general write function to write a schme or a section
writeSecOrSchm ::  SecOrSchm -> IO()
writeSecOrSchm  secScm = do
    doesDirectoryExist sectDir >>= \yn -> if yn then return () else createDirectory sectDir
    case secScm of
        Left  sec -> L.writeFile outPath . L.pack $ section2String sec
        Right scm -> L.writeFile outPath . L.pack $ scheme2String scm
    exist <- doesFileExist resultList
    if exist then do-- (++"\n").
        -- replace any previous file with that name
        ---results <- liftM (filter (/= outFile) . L.lines) $ L.readFile resultList
        --let newResults = L.intercalate (L.pack "\n") (outFile : results)
        L.appendFile resultList (L.snoc outFile '\n') --- newResults
    else
        L.writeFile resultList (L.snoc outFile '\n')
    where
        outFile  = (L.pack .  takeWhile (/= '.') $ drop (length sectDir) outPath)
        --
        sectDir  = "./SectionResults/"
        --"./SectionResults/results.txt"
        resultList = sectDir ++"results.txt"
        --
        outPath  = (fPath secScm)
        fPath :: SecOrSchm -> String
        fPath sc =
            case sc of
                Left  sec -> do
                    let pttLs  =  [show $ sCondition sec, sScannerCode sec, sScannerSorrg sec, sCWay sec]
                    let path =   intercalate "_" pttLs
                    sectDir ++ path ++ ".txt"
                Right scm -> sectDir ++ (schemeName scm) ++ ".txt"
----------------------------------------------------------------------------------------------------
{--
sCondition     :: Defect, -- ^ the condition being analysed for the section
    sScannerCode   :: String, -- ^ the scanner section code
    sScannerSorrg  :: String, -- ^ the scanner section sorrogate
    sCWay
--}
--- reading and writing to files
----------------------------------------------------------------------------------------------------
-- separates a bytestring at given blocks, removeing the intervening block
separateAt :: [L.ByteString] -> L.ByteString -> [L.ByteString]
separateAt  []  _   = []
separateAt  hs  bs  = maybe [] (\_ -> tail noChulks) (listToMaybe noChulks)
    where
        noChulks = map (L.filter (/= '\n')) $ separateAt_aux hs bs
        isValid a     = isDigit a || a == '.' || a == ','
        --
        separateAt_aux        []         _     = []
        separateAt_aux (block : remainder) bs  = front : separateAt_aux remainder back
            where
                (front, back) = splitAtChunkBS block bs

--- maybe2: returns Just if both maybes are a Just or nothing otherwise
maybe2 :: Maybe a -> Maybe b -> Maybe (a,b)
maybe2 =  liftM2 (,)

maybe3 :: Maybe a -> Maybe b -> Maybe c -> Maybe (a,(b,c))
maybe3 a b c =  maybe2 a (maybe2 b c)

maybe4 ::  Maybe a -> Maybe b -> Maybe c -> Maybe d ->  Maybe ((a,b),(c,d))
maybe4  a b c d  =  maybe2 (maybe2 a b) (maybe2 c d)


maybe5 ::  Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe (a,((b,c),(d,e)))
maybe5 a b c d e =  maybe2 a (maybe2 (maybe2 b c) (maybe2 d e))

maybe7 ::  Maybe a -> Maybe b -> Maybe c -> Maybe d ->
           Maybe e -> Maybe m -> Maybe n -> Maybe ( (a,((b,c),(d,e))), (m,n))
maybe7 a b c d e m n = maybe2  (maybe5 a b c d e) (maybe2 m n)
--
readResultRec :: FilePath -> IO (Maybe ResultRec)
readResultRec fileName = L.readFile fileName >>= return  . getResultRec . separateAt rpars
    --print (show contents)
    --maybe (print "nothing") (print . printResultsRec) (getResultRec contents)
    where
        rpars = map L.pack ["alp: ", "std: ","ste: ","abr: ","rprPredictions: ","rprCuts: ", "endOfResults"]

{--  converts a list of bytestring to a ResultRec
sResults: alp: 0.0
std: 0.0
ste: 0.0
abr: 1.0
rprPredictions: 0.0
rprCuts: 0
---}
getResultRec :: [L.ByteString] -> Maybe ResultRec
getResultRec =  getRRec . map (L.filter (not . isSpace))--- ((=<<) ignoreDummy) .
    where
        --ignoreDummy (
        bs2Doublek = bs2Double1 . L.filter (not . isSpace)
        --bs2Intt    = bs2Int' . L.filter (not . isSpace)
        --
        ignoreDummy  (ResultRec a b c d e k)
            | all (== 0) [a,b,c] = Nothing
            | otherwise          = Just (ResultRec a b c d e k)
        --len = lenght xs
        check5 xs = maybe5 (bs2Doublek (xs !! 0))
                           (bs2Doublek (xs !! 1))
                           (bs2Doublek (xs !! 2))
                           (sequence $  map bs2Doublek (L.split ',' (xs !! 3)))
                           (sequence $ map bs2Doublek (L.split ',' (xs !! 4)))
        check6 xs = maybe2 (check5 xs) (sequence $ map bs2Int' (L.split ',' (xs !! 5)))
        --
        getRRec ks
            | length ks == 5  =
                case check5 ks of
                    Nothing -> Nothing
                    Just (a,((b,c),(d,e))) -> Just $ ResultRec a b c d e []
            | length ks == 6  =
                case check6 ks of
                    Nothing ->  Nothing
                    Just ((a,((b,c),(d,e))),m) -> Just $ ResultRec a b c d e m
            | otherwise       =  Nothing

-- read a Result record from a file. Retuns Nothing is there is an error in the record
readResults :: FilePath -> IO (Maybe Results)
readResults = liftM readResults_aux .  L.readFile

readResults_aux :: L.ByteString -> Maybe ([ResultRec] , [Int])
readResults_aux contents = liftM (\a ->  (a,tdsCutsList)) $ sequence rblocks
    where
        rblocks      = map (readResultR . takeB4Block tdsC) $ splitAtChunksBSx alP  contents
        tdsCutsList  = getInts . takeB4Block eoR $  dropB4Block tdsC contents
        --
        tdsC         = L.pack "tdsCuts: "
        alP          = L.pack "alp: "
        eoR          = L.pack "endOfResults"
        --
        readResultR :: L.ByteString -> Maybe ResultRec
        readResultR  =   getResultRec . separateAt rpars --  Just getInts )
        --
        rpars        = map L.pack ["alp:", "std:","ste:","abr:",
                                   "rprPredictions:","rprCuts:", "endOfResults"]
        --
        tdsprs =  map L.pack ["tdsCuts: ","endOfResults" ]
        --intsR  = L.split ',' $ L.concat $ separateAt tdsprs bs
        -- bsd       = bs2Double1 . L.filter (not . isSpace)
        mkPos  n = if n >= 0 then n else (-1)*n
        bs2Int1  = liftM mkPos . bs2Int' . L.filter (not . isSpace)
        getInts xs  = [ x |  k <- map bs2Int1 (L.split ',' xs), isJust k , let Just x = k]


--
readSectionTest = do
    --let cpars = map L.pack ["cInterval: ", "cData: ","cYears: ","cMissingYrs: "]
    --rsect <- L.readFile "./sectTest.txt" "./SectionResults/LLTX.txt" --
    rsect <- readSection "./SectionResults/LGRDCR1.txt" -- "./sectTest.txt" --
    --
    let stC = L.pack "cInterval: "
    let eoC = L.pack "EOChn"
    --let seps = map (map (takeB4Block eoC) . separateAt cpars) $ splitAtChunksBSx stC rsect
    case rsect of
       Nothing       -> print "read error"
       Just sections ->  print (show sections)
    --print (show $  map (map L.unpack) seps)

--- read a chain record from a file
readChain :: FilePath -> IO (Maybe [Chain])
readChain = liftM  readChain_aux  . L.readFile

readChain_aux :: L.ByteString -> Maybe [Chain]
readChain_aux  =   sequence . sblocks
    where
        sblocks = map readChainsR . splitAtChunksBSx stC --  $ takeB4Block eoC contents.
        --
        stC = L.pack "cInterval: "
        eoC = L.pack "EOChn"
        --
        cpars = map L.pack ["cInterval: ", "cData: ","cYears: ","cMissingYrs: "]
        --
        readChainsR :: L.ByteString -> Maybe Chain
        readChainsR =    getChainRec . map (takeB4Block eoC) . separateAt cpars
        ----  converts a list of bytestring to a Chain record
        getChainRec :: [L.ByteString] -> Maybe Chain
        getChainRec ks
            | length ks == 4  =
                case check4 ks of
                    Nothing -> Nothing
                    Just ((b,(c,d)),e) ->
                       if length b >= 2 then
                          Just $ Chain (b!! 0, b!! 1) c d e
                       else Nothing
            | length ks == 3  =
                case check3 ks of
                    Nothing -> Nothing
                    Just (a,(b,c)) ->
                        if length a >= 2 then
                            Just $ Chain (a !! 0, a!! 1) b c []
                        else Nothing
            | otherwise       = Nothing
            where
                bsd       = bs2Double1 . L.filter (not . isSpace)
                check3 xs = maybe3 (sequence $ map bsd (L.split '-' (xs !! 0)))
                                   (sequence $ map bsd (L.split ',' (xs !! 1)))
                                   (sequence $ map bsd (L.split ',' (xs !! 2)))
                check4 xs = maybe2 (check3 xs) (sequence $ map bs2Double1 (L.split ',' (xs !! 4)))

---
readResultsPerChain :: FilePath -> IO (Maybe [(Chain,ResultRec)])
readResultsPerChain filePath = do
    chains <- readChain filePath
    results <- readResults filePath -- :: FilePath -> IO (Maybe Results)
    return . liftM (uncurry resultsPerChain) $ maybe2 chains results
    --    Just (chn, rts) ->

--resultsPerChain :: [Chain] -> Results ->  [(Chain,ResultRec)]

-- read a section records form a file
readSection :: FilePath -> IO (Maybe Section)
readSection =  liftM readSection_aux . L.readFile

readSection_aux :: L.ByteString -> Maybe Section
readSection_aux  = readSectionR
    where
        eoSec = L.pack "endOfResults" --"EOSec"
        --
        spars = map L.pack ["sCondition: ", "sScannerCode: ","sScannerSorrg: ",
                            "sCWay: ", "sChains: ", "sResults: ","endOfResults"]
        --
        readSectionR :: L.ByteString -> Maybe Section
        readSectionR =  getSectionRec .  separateAt spars . takeB4Block eoSec
        ----  converts a list of bytestring to a Section record
        getSectionRec :: [L.ByteString] -> Maybe Section
        getSectionRec ks
            | length ks == 6  =
                case check6 ks of
                    Nothing ->
                        case check5 ks of
                            Nothing -> Nothing
                            Just (a,((b,c),(d,e))) ->  Just $ Section a b c d e ([],[])
                    Just ((a,((b,c),(d,e))),k) -> Just $ Section a b c d e k -- (k,[])
            -- | length ks == 5  =
            --    case check5 ks of
            --        Nothing -> Nothing
            --        Just (a,((b,c),(d,e))) ->  Just $ Section a b c d e ([],[])
            | otherwise       = Nothing
            where
                check5 xs = maybe5 (string2SCODE . filter (not. isSpace) $ L.unpack (xs !! 0))
                                   (liftM show   $ bs2Integer   (xs !! 1))
                                   (liftM show   $ bs2Integer  (xs !! 2))
                                   (liftM L.unpack $ notNul (xs !! 3))
                                   (readChain_aux (xs !! 4))
                                   --
                check6 xs = maybe2 (check5 xs) (readResults_aux (xs !! 5))
                notNul xs
                    | L.null xs   = Nothing
                    | otherwise   = Just xs


---
eoSchmRec  = L.pack "EOSchemeRec"
-- read a schemeRec recfrom a file
readSchemeRec :: FilePath -> IO (Maybe [SchemeRec])
readSchemeRec =  liftM readSchemeRec_aux . L.readFile

readSchemeRec_aux :: L.ByteString -> Maybe [SchemeRec]
readSchemeRec_aux  = sequence . sblocks
    where
        scmC   = L.pack "scmScannerCode: "
        sblocks = map readSchemesR . splitAtChunksBSx scmC
        --
        scmPrs = map L.pack ["scmScannerCode: ", "roadClassCode: ","roadName: ","scmScannerSorrg: ",
                              "startChain: ","endChain: ","scmChains: ","scmResults: "]
        --
        readSchemesR :: L.ByteString -> Maybe SchemeRec
        readSchemesR =    getSchemesRec . map (takeB4Block eoSchmRec) . separateAt scmPrs
        ----  converts a list of bytestring to a SchemeRec record
        getSchemesRec :: [L.ByteString] -> Maybe SchemeRec
        getSchemesRec ks
             | length ks == 8  =
                case check8 ks of
                    Nothing -> Nothing
                    Just (((a,((b,c),(d,e))), (n,m)), r)  ->
                        if  (all ( >= 2) . map length) [e,n]  then
                            Just $ SchemeRec a b c d (e !! 0, e !!1) (n !! 0, n !! 1) m r
                        else
                            Nothing
            | length ks == 7  =
                case check7 ks of
                    Nothing -> Nothing
                    Just ( (a,((b,c),(d,e))), (n,m))  ->
                        if   (all ( >= 2) . map length) [e,n]  then
                            Just $ SchemeRec a b c d (e !! 0, e !!1) (n !! 0, n !! 1) m ([],[])
                        else
                            Nothing
            | otherwise       = Nothing
            where
                check7 xs = maybe7 (liftM show   $ bs2Integer  (xs !! 0))
                                   (Just   $ L.unpack  (xs !! 1))
                                   (Just   $ L.unpack  (xs !! 2))
                                   (liftM show   $ bs2Integer  (xs !! 3))
                                   (sequence $ map bs2Double1 (L.split '-' (xs !! 4)))
                                   (sequence $ map bs2Double1 (L.split '-' (xs !! 5)))
                                   (readChain_aux (xs !! 6))
                                   --
                check8 xs = maybe2 (check7 xs) (readResults_aux (xs !! 7))
---
eoSchm     = L.pack "EOScheme"

-- read a schemeRec recfrom a file
readScheme :: FilePath -> IO (Maybe Scheme)
readScheme =  liftM readScheme_aux . L.readFile

readScheme_aux :: L.ByteString -> Maybe Scheme
readScheme_aux  = readSchemeR -- sblocks --sequence . sblocks
    where
        --scmC    = L.pack "schemeName: "
        --sblocks =  -- . splitAtChunksBSx scmC
        scmPrs  = map L.pack ["schemeName: ", "schmCondition: ","schmRecs: "]
        --
        readSchemeR :: L.ByteString -> Maybe Scheme
        readSchemeR =    getSchemeRec . map (takeB4Block eoSchm) . separateAt scmPrs
        --
        getSchemeRec :: [L.ByteString] -> Maybe Scheme
        getSchemeRec ks
            | length  ks == 3 =
                case check3 ks of
                    Nothing -> Nothing
                    Just (a,(b,c)) -> Just $ Scheme a b c
            -- | length  ks == 2 =
            --    case check2 =
            | otherwise = Nothing
            where
                check3 xs = maybe3 (Just   $ L.unpack (xs !! 0) )
                                   (string2SCODE . filter (not. isSpace) $ L.unpack (xs !! 1))
                                   (readSchemeRec_aux (xs !! 2))

{------------------------------------------------------------------------------------------------
--- Now to define conversion functions:
    from HMDIF intermediary to a lists of schemes and Sections. Indeed, if the type have not
    been analysed, the result rec would be empty
------------------------------------------------------------------------------------------------}

-- now can read all the sections from the HMDIF intermediary
--
readSectionsFromOutFile :: Maybe (String, String) -> FilePath -> IO (Maybe [Section])
readSectionsFromOutFile  mSSrg =
    liftM (liftM (mmMiss . segSorrgF) .  mapM bsList2Section . groupSections) . L.readFile
    where
        getSegSorg (a,b) seg = (a == (sScannerCode seg)) -- && (b == (sScannerSorrg seg))
        segSorrgF   :: [Section] -> [Section]
        segSorrgF     xs       =  maybe xs  (\a -> filter (getSegSorg a) xs ) mSSrg
        mmMiss    xs    =  [seg{sChains = adjust scn} | seg <- xs
                                                        , let scn = sChains seg
                                                        , let fC      = fst . cInterval
                                                        , let adjust = nubBy (\a b -> (fC a == fC b)) ]
                                                                       -- calcMissing ]
-- scanner: [SECTION\40070,529207007,1080,14112005,14112005,,;OBSERV\LLRT,CL1,0,5;OBVAL\13,,1.2,V;]
-- scrim: [SECTION\UKMAN,40070,529207007,1080,290712,290712,,;OBSERV\SFC,1,CR1,0,10;OBVAL\12,,0.72,V;]
-----
groupSections ::  L.ByteString -> [[L.ByteString]]
groupSections fileContents =  sepBySect fileContents
    where -- groupBySectAndSorg .concatMap
        sepBySect =   map (splitAtChunksBSx sectionDelimiter) . getSlist
        --
        ret       = L.pack "===-----------result section------------====="
        dashes    = L.pack "-----"
        retainData    = map (removeBlock dashes . removeBlock ret)
        --
        getSlist  = filter (not . null . L.lines) . retainData . splitAtChunksBSx ret
        -- SECTION\40070,529207007,1080,OBSERV\LLRT,CL1,0.0,10.0
        groupBySectAndSorg :: [L.ByteString] -> [[L.ByteString]]
        groupBySectAndSorg    []   = []
        groupBySectAndSorg  (x:xs) = ((x: front) : groupBySectAndSorg back) `using` (parList rseq)
            where
                (front,back) = partition sectSorg xs
                sectSorg y   = fty == ftx -- && eqSSorrg fty  == eqSSorrg  ftx
                    where
                        fty  = first2 y
                        ftx  = first2 x
                first2       =  take 2 . L.split ',' . head . L.lines

-- takes a list of bytestrings representing all the data in a given section and
-- turns it into a section representation. There are no results in the section
bsList2Section :: [L.ByteString] -> Maybe Section
bsList2Section []          =       Nothing
bsList2Section bs@(a:_)    =
    case segRefs a of
        (Nothing, _ , _ , _)    -> Nothing
        (Just a , b , c, d)     ->
            case (mapM mkChain . filter (not . L.null) ) bs of
                Nothing         -> Nothing
                Just chn        -> Just $ Section a b c d chn ([],[])

    where
        --
        segRefs        =  conCodeDorg . head . L.lines
        conCodeDorg xs = (con, code, sorg, cway)
            where
                -- ls        = L.split ',' xs
                dropB4Block1    block   =  snd . splitAtChunkBS block
                nnCm   n  ns
                    | L.null mm  =  nn
                    | otherwise  =  L.tail mm
                    where (nn,mm) = L.break (== '\\') $  nthComma' n xs
                 --   | otherwise           = nthComma' n bs
                con       = string2SCODE . L.unpack $  nnCm 4 xs --   (ls !! 3)
                code      = L.unpack $ nnCm 1 xs --  L.tail . snd $ L.break ( == '\\') (ls !! 0)
                sorg      = L.unpack $ nthComma' 2 xs -- (ls !! 1)
                cway      = L.unpack $ nthComma' 5 xs -- (ls !! 4)


{-
SECTION\40070,529207007,1080,OBSERV\LLRT,CL1,20.0,30.0
13092004,13092004,3.9
14112005,14112005,1.0
13092007,13092007,3.3
26052009,26052009,2.8
11042011,11042011,3.1
-----
SECTION\40070,529207007,1080,OBSERV\LLRT,CL1,30.0,40.0
13092004,13092004,4.4
14112005,14112005,1.7
13092007,13092007,3.3
26052009,26052009,3.2
11042011,11042011,3.6
   tail . L.lines
[a,b,c] -> (a,c)
Chain = Chain {
    cInterval :: (Double, Double),
    cData :: [Double],
    cYears :: [Double],
    cMissingYrs :: [Double]
    } deriving (Show, Eq)

mkChain :: L.ByteString -> Maybe Chain
mkChain    = liftM setChain . invAndDats . filter (not . null) . chnList
    where
        fromChn   = removeBlock (L.pack "CL1,") . removeBlock (L.pack "CR1,")
        chnList   = map (L.split ',') . L.lines . fromChn
        --isValid ys = length front == 2 && all ((== 3) . lenght) rest
        --    where (front, rest) = (head ys, tail ys)
        yearsAndData xs  = maybe2 (bs2Double1 . L.drop 4 $ head xs) (bs2Double1 $ last xs)
        getIntervals xs  = maybe2 (bs2Double1 (xs !! 4)) (bs2Double1 (xs !! 5))
        invAndDats xs    = maybe2 (getIntervals $ head xs) (sequence . map yearsAndData $ tail xs)
        --
        setChain (intv, yrsdta) = Chain intv dta yrs []
            where
                (yrs, dta)    = unzip yrsdta
                -- (yrs', dta')  = adjustValues dta yrs
                -- this should be done in a separate function

--}
-- takes a byteString and creates a chains record
mkChain :: L.ByteString -> Maybe Chain
mkChain bs
    | null ns       = Nothing
    | otherwise     = Just $ Chain intv dta yrs []
    where
        ns            = L.lines bs
        doubleN n     = bs2Double . nthComma' n
        (front, rest) = (head ns, tail ns)
        intv          = (doubleN 6 front, doubleN 7 front)
        cdYs          = [(yr1,bs2Double val) | cbs <- rest , let ks = L.split ',' cbs
                                    , length ks == 3
                                    , let [sy,_,val] = ks
                                    , let yr = bs2Double $ L.drop 4 sy
                                    , let yr1 = if yr < 100 then 2000 + yr else yr
                                    ]
        (yrs,dta)     = unzip cdYs

-- calculate missing values in a chain, and fillup those that
-- are missing wit real data

calcMissing :: Maybe [([Double] -> [Double])] -> [Chain] -> [Chain]
calcMissing mXs chN  =
    case mXs of
        Nothing -> map (flip setMissing Nothing)  chN
        Just kss  -> [setMissing chn (Just f)  | (chn, f) <- zip chN kss]
    -- get the lowest and highest values for years
    where
        setMissing chn mmN = Chain (cInterval chn) ndta nyrs (map fromJust $ filter isJust ms)
            where
               (yds, ms) =  unzip $ setYd chn
               --(dta, yrs) = unzip yds
               (ndta, nyrs) =  insertMissing  (cData chn) (cYears chn)  (minYr,maxYr) mmN
        -- need to make this more robust
        mMinimum xs = maybe 0 (\_ -> minimum xs) (listToMaybe xs)
        mMaximum xs = maybe 0 (\_ -> maximum xs) (listToMaybe xs)
        --
        minYr = mMinimum $ map (mMinimum . cYears) chN
        maxYr = mMaximum $ map (mMaximum . cYears) chN
        --
        setYd xs = [ ks  | ((a,b),c) <- zip (zip ds ys) [minYr  .. maxYr ]
                     , let ks = if b == c then ((a,b), Nothing) else ((missingVal,c), Just c) ]
            where
                ds = cData xs
                ys = cYears xs
        --- fill up the missing calues
--}
{--

insertMissing :: [Double] -> [Double] -> (Double,Double) -> ([Double],[Double])
insertMissing  values years (mn,mx) = unzip inserted
--}
--- get an OutFile and returns a section
outfile2Section_aux :: FilePath -> IO (Maybe [Section])
outfile2Section_aux fileName =
    if isOutFile fileName then
       liftM (sequence . map bsList2Section  . groupSections ) $ L.readFile fileName
    else
       return Nothing
    where
        isOutFile fName =
            front == "out"  && (isJust . string2SCODE) mid && back == ".txt"
            where
                front = take 2 fName
                mid   = takeWhile ( /= '.') $ drop 3 fName
                back  = dropWhile ( /= '.') fName

