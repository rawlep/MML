module FileIO (getAlreadyExtracted
    ,getSectionsAndSorrogate
    ,getSectionsAndSorrogateS
    ,getSectionData
    ---------------------------- extracted files ------
    ,checkIfExtracted
    ,removeExtracted
    ,updateExtracted
    ,mkGroupsFromResults
    ,readSchemeResults
    ----------------------------------------------------
    ,csv2HMDIF_aux
    ) where
-- library functions
import qualified Data.ByteString.Lazy.Char8 as L
import Numeric.LinearAlgebra (fromList)
--
import Control.Monad (liftM, liftM2)
import System.Directory (doesFileExist, doesDirectoryExist,createDirectory,removeFile)
import Data.Char (isDigit, isAlpha,isSpace,isAlphaNum)
import Control.Parallel.Strategies (using, parList, rpar, rseq)--  as St-- (parMap)
import Data.Maybe (isJust,listToMaybe)
import System.Random (randomRs,newStdGen)
import Data.List (sortBy,groupBy,partition,unzip3, find,transpose,
                  delete,foldl',nub, nubBy,deleteBy,sort) --

-- appliation functions
import LMisc (findIndexesPresent,mkIntervals)
import GlobalParameters (RResults, Results (..))
import ProcessFile (Display, display,writeOrAppend) -- ,bs2Double') -- ,Alignment(..))
import HMDIFPrelude -- hiding (bs2Double)
import RprMix2 (Mmodel (MkModel), r_n)
---------------------------
maybe2 :: Maybe a -> Maybe b -> Maybe (a,b)
maybe2 =  liftM2 (,)

bs2Double' = Just . bs2Double


mapMPair :: Monad m => (a -> m b) -> [(a,c)] -> m [(b,c)]
mapMPair  f xs = sequence [liftM2 ( ,) (f a) (return c)  | (a,c) <- xs ]
---}
--isDigit, isAlpha

removeExtracted :: Bool -> IO ()
removeExtracted tf = do
    let extracted = if tf then extractFile else resultFile
    exist <- doesFileExist extracted
    if exist then removeFile extracted else (return ())
---------------------------------------------------------------------------------------
---     CHECKING FOR FILES THAT HAVE BEEN EXTRACTED OR RESULTS THAT ARE AVAILABLE
extractFile = "./Extracted/extracted.txt"
resultFile = "./Extracted/results.txt"
eDir = "./Extracted/"

updateExtracted :: String -> IO ()
updateExtracted parm =  do
    doesDirectoryExist eDir >>= \yn -> if yn then return () else createDirectory eDir
    writeOrAppend extractFile (L.pack (parm ++"\n"))

checkIfExtracted :: Bool -> Bool -> (String -> IO()) -> IO ()
checkIfExtracted tf  outputTyp write = do
    let def  = "./Defects/"
    doesDirectoryExist eDir >>= \yn -> if yn then return () else createDirectory eDir
    let ftyp = if outputTyp then "out" else "result"
    let !cr1 = map (((def ++ ftyp) ++) . (++"-CR1.txt") . show) conditions
    let !cl1 = map (((def ++ ftyp) ++) . (++"-CL1.txt") . show) conditions
    mapM_ (checkFile write) (cr1 ++ cl1)
    where
        checkFile prt fileName = do
            exist <- doesFileExist fileName
            let nm = if outputTyp then 13 else 16
            let defect = takeWhile (/= '.') $ drop nm fileName
            let fName = if outputTyp then extractFile else resultFile
            if exist then do
                if tf then
                    writeOrAppend fName (L.pack (defect ++"\n"))
                else
                    return ()
                prt ("Defect type "++ defect ++ " already extracted")
            else
                return ()


-- getting already-extracted conditions or results that have already beed calculated
-- The function is called with True for extractions and False for results
getAlreadyExtracted :: Bool -> IO [String]
getAlreadyExtracted tf  = do
    -- this is for condition. Those for schemes will be prefixed with a SCM tag
    --let unpackP (a,b) = L.unpack a ++"-" ++ L.unpack b
    let extractOrResult = if tf then extractFile  else resultFile
    let string          = if tf then "Nothing Extracted" else "No Results"
    let getCondition =  map L.unpack
    let indxFrom ls  = findIndexesPresent id  ls conditionsCR
    fileThere <- doesFileExist extractOrResult
    if fileThere then do
    --- note the extract file should have a - between the condition and the carriage way
    --- example, LTEX-CR1
        slist  <- L.readFile extractOrResult  >>= return . indxFrom . getCondition . L.lines
        -- let indexes = findIndexesPresent id slist (map show conditions)
        return [condCR ++ desp  | i <- slist,
                                 let condCR = conditionsCR !! i ,
                                 let (con, cway) = span (/= '-') condCR,
                                 let dispIndex = find ((== con) . show) conditions,
                                 isJust dispIndex , let Just ds = dispIndex ,
                                 let desp = dropWhile (/= ' ') (display ds)  ]
    else
        return [] --string]
---------------------------------------------------------------------------------------------------
delim       = L.pack "===-----------result section------------====="
dashes      = L.pack "-----"
--remDlt      = map (fst . splitAtChunkBS delim)
remDlt      = map (fst . splitAtChunkBS dashes .  fst . splitAtChunkBS delim)
-- get data for a choses section from an outFile
type Years = Double
type OBs   = Double
--type Defect = String
----------------------------------------------
getSectionData ::  String -> String -> FilePath -> IO [(String, [(Double,OBs)])]
getSectionData section sSorr fileName = do
    L.readFile fileName >>= return . getSectionData_aux section sSorr

getSectionData_aux :: String -> String -> L.ByteString -> [(String, [(Double,OBs)])]
getSectionData_aux section sSorr  =  nub . vals
    where
        sectSorr    = L.pack sSorr
        sectionName = sectionDelimiter <> bSlash <> (L.pack section)
        getYrVls bs =  (bs2Double' . L.drop 4 $ nthComma 1 bs, bs2Double' $ nthComma 3 bs)
        vals bs     = [ (chain, ydata)  | vals <-  (remDlt . splitAtChunksBSx sectionName) bs,
                           subStringB sectSorr vals,
                           let lvals = L.lines vals , not $ null lvals,
                           --et lvals' = init lvals,
                           let (top, rest)  = (head lvals,  tail lvals) ,
                           let chain = L.unpack (nthComma 6 top) ++"-"++ L.unpack (nthComma 7 top) ,
                           let ydata' = filter isJust $ map (uncurry maybe2 . getYrVls) rest,
                           let Just ydata = sequence ydata' , not $ null ydata ]
    --L.readFile fileName >>= return . nub . vals

-- get all the sections and the sorrogate names
-- getSectionsAndSorrogate, getSectionData
getSectionsAndSorrogateS  =
    liftM (map (\(a,b) -> L.unpack a ++" -- " ++ L.unpack b) )  . getSectionsAndSorrogate

getSectionsAndSorrogate :: String -> IO [(L.ByteString, L.ByteString)]
getSectionsAndSorrogate fileName =
    L.readFile fileName >>= return .  getSectionsAndSorrogate_aux

getSectionsAndSorrogate_aux :: L.ByteString ->  [(L.ByteString, L.ByteString)]
getSectionsAndSorrogate_aux  = nub . sectS
    where
        sDelm = L.snoc sectionDelimiter '\\'
        getSD bs =  (nthComma 1 bs,  nthComma 2 bs)
        sectS bs = [getSD (head sect) | vals <- (remDlt . splitAtChunksBS sDelm) bs,
                       let sect = L.lines vals , not $ null sect]

--------------------------------------------------------------------------------------------------
--- read the fiile with the
getDataFromOutFile :: FilePath -> IO [(String , [(String, [(Double,OBs)])])]
getDataFromOutFile fileName = do
    sections <- L.readFile fileName >>= return . splitAtChunksBS delim
    let ssOrgs = getSSrgs sections
    let getSectDat  (a , b)  = (a++" -- "++b , getSectionData_aux a b)
    return $ zipWith apply (map getSectDat ssOrgs) sections
    where
        getSSrgs          =  map (map2pair L.unpack) . concatMap  getSectionsAndSorrogate_aux
        map2pair f  (a,b) = (f a, f b)
        apply (b, f) a    = (b, f a)

--- read the reslt and raw data for a profile
mkGroupsFromResults :: String -> IO [((String , [(String, [(Double,OBs)])]), RResults ) ]
mkGroupsFromResults defect = do
    let defectFile = "./Defects/" ++ "out"++defect++".txt"
    let resultFile = "./Defects/" ++"result"++defect++".txt"
    origData <- getDataFromOutFile defectFile
    results <- readAnalResults resultFile
    ---- seperate groups in the input list based on the cuts
    let groupPlaces  =  [((sRef, concatMap fst grp),rs) | ((sRef, sdata), rs) <- zip origData results
                                               , let grp = mkIntervals sdata (snd rs) ]
    -- let groupPlaces =  map fst $  mkIntervals origData (map (sum . snd) results)
    return groupPlaces -- (zip groupPlaces results)
{--
--- [(String , [(String, [(Double,OBs)])])]  -> RResults ->  DrwRef


--}
--------------------------------------------------------------------------------------------------
-----------  readinig the analysis results -------------------------------------------------------
{--
alp:0.8833167284811266
std:1.1778715984349453
ste:1.4511686889046098
abr:2.940244520146411,1.911507322425139e-2,0.5314487016655314,0.4907857621801883
tau:1.262116902934248
RpRpredictions: 2.244681499949299,2.4363579626193954,2.6280344252894916,2.8197108879595882,3.0113873506296844,3.2030638132997806,3.394740275969877,3.5864167386399735,2.69889371255129,2.774906914964689,2.8509201173780876,2.9269333197914866,3.0029465222048857,3.0789597246182843,3.1549729270316833,3.2309861294450823
RprCutsFreq: 8
tdsCuts:
--}

--- read analysis results
readAnalResults :: FilePath -> IO [RResults]
readAnalResults  fileName =
    L.readFile fileName >>= return . readAnalResults_aux

{-- results for one dataType
readAnalAllResults :: FilePath -> IO [[Results]]
readAnalAllResults fileName = do
    let resultString = L.pack "=============== MML2DS Results OMods ===================="
    L.readFile fileName >>= return . map readAnalResults_aux . splitAtChunksBS resultString
--}


readAnalResults_aux :: L.ByteString -> [RResults]
readAnalResults_aux  = sectS
    where
        resultSep = L.pack "----------------Progression Group --------------------------"
        reOMd = L.pack "=============== MML2DS Results OMods ===================="
        tdsCt     = L.pack "tdsCuts:"
        abrColon  = L.pack "alp:"
        sepFromBack = splitAtChunkBS tdsCt
        bsd  = bs2Double . L.filter (not . isSpace)
        ints = bs2Int' . L.filter (not . isSpace)
        sepAt c  = last . L.split c
        sectS bs = [rslt' | vals <- (map (fst .  splitAtChunkBS reOMd) . splitAtChunksBS resultSep) bs
                          ,let (front,tdscuts') = splitAtChunkBS tdsCt vals
                          -- ,let (tdscuts',_)    = splitAtChunkBS reOMd tdscuts
                          ,let abrs =  splitAtChunksBSx abrColon front
                          ,let sect =  map (filter (/= L.empty) . L.lines) abrs
                          ,all ((== 7) . length) sect
                          ,let xss =  map (map (sepAt ':')) sect  -- [a,b,c,d,e,f,g,h]
                          ,let bsds = map bsd . L.split ','
                          ,let intss xs = [k | i <- map ints $ L.split ',' xs, isJust i,
                                             let Just k = i]
                          ,let rslt [a,b,c,d,e,f,g] = Results (bsd a) (bsd b) (bsd c) (bsds d) (bsd e) (bsds f) (intss g)
                          ,let rslt' = (map rslt xss , intss tdscuts')
                          --}
                          ]

-- readAnalResults "./Defects/outLLRDCL1.txt"
-- read files for schemeresults
readSchemeResults :: FilePath -> IO (Maybe ([(String, [(Double,Double)])] ,   RResults,[Double]))
readSchemeResults fileName = do
    rrslts <- readAnalResults fileName
    (chainsYrs, dataa) <- readSchemeResults_aux fileName
    maybe (return Nothing)
          (\a -> do
                let lens = map (length . rpRpredictions)  (fst a)
                let (yss,_ ) = unzip (mkIntervals dataa lens)
                let probs  = concat (zipWith getAlphs (fst a) yss)
                return $ Just (chainsYrs , a, probs))
          (listToMaybe rrslts)
    ----------------------------------
    where
        getAlphs :: Results -> [Double] -> [Double]
        getAlphs rlt  dst = zipWith calcProb (prepForMod rlt) dst
        -- where
        --prepForMod :: Results -> [Double]
        prepForMod  rs = map (\a -> (alp rs, std rs, ste rs, a)) (rpRpredictions rs)
        --
        calcProb (alp, sd , se, prd) org = rAg (r_n modl prd org)
            where
                kk   = [0.001,0.0778461 .. 1]
                modl = MkModel alp sd se (fromList [0]) 0
                rAg x | x <= kk !! 0 = 0 -- red
                      | x <= kk !! 1 = 1 -- red
                      | x <= kk !! 2 = 2
                      | x <= kk !! 3  = 3
                      | x <= kk !! 4 = 4 --
                      | x  <= kk !! 5 = 5
                      | x >= kk !! 6  && x <= kk !! 7 = 6 -- white
                      | x <= kk !! 8 = 7
                      | x <= kk !! 9 = 8
                      | x <= kk !! 10 = 9
                      | x <= kk !! 11 = 10 -- amber
                      | x <= kk !! 12 = 11
                      -- | x <= kk !! 13 = 10
                      | otherwise = 12  -- blue

readSchemeResults_aux :: FilePath -> IO ([(String, [(Double,Double)])],[Double])
readSchemeResults_aux filename = do
    cont <- L.readFile filename
    let (vals , _ ) = splitAtChunkBS resultSep cont
    let frt      =  L.filter (not . isSpace) vals -- . L.takeWhile (/= ']')) cont
    let (front,rest) = splitAtChunkBS (L.pack "years:") frt
    let (middle, back) = splitAtChunkBS (L.pack "data:") rest
    let years   = L.split ',' (yrVals middle)
    let chains  = L.split ',' (chainVals front)
    let dataa  = map  bs2Double $ L.split ',' (dataVals back)
    --print (L.unpack front)
    --print (L.unpack back)
    return $ (sect years chains, dataa)
    where
        resultSep = L.pack "----------------Progression Group --------------------------"
        brace a   = a == '[' || a == ']'
        yrV   a   = any (== a) "years:"
        chnV  a   = any (== a) "chains:"
        datV  a   = any (== a) "data:"
        yrVals    = L.filter (not . brace) -- . L.dropWhile yrV
        chainVals = L.dropWhile chnV . L.filter (/= '"')
        dataVals = L.dropWhile datV . L.filter (/= '"')
        ----------------------------------
        sect ys chs  = [(chn , zip yrs [1,2 .. ] ) | let yrs = map  bs2Double ys
                         -- let front = L.takeWhile (/= ']') bs -- , back) = splitAtChunkBS resultSep bs
                         --  let chYrs  = L.lines . L.filter (not . isSpace)
                         --, let chs    = chYrs bs
                         -- ,
                         , chn <-  map L.unpack chs   ]
        ----------------------------
        --years cs | null cs   =  []
        --         | otherwise =  L.split ',' (yrVals (cs !! 1))
        --      where
        --
        ---
        --chains cs | null cs     =  []
        --          | otherwise   =  L.split ',' (chainVals (cs !! 0))
        --       where

{-
chains:"0.0-10.0","10.0-20.0","20.0-30.0","30.0-40.0","40.0-50.0"
years:[2006.0,2010.0,2004.0,2008.0,2012.0]
-}

-------------------------------------------------------------------------------------------
-- converting a comma-seperated .csv file ot a HMDIF file. The rows in the file refer to
-- the chainage, while the columns refer to the number of years
-------------------------------------------------------------------------------------------
--type CsvFile = L.ByteString
type HMDheader = L.ByteString
type CWay = L.ByteString
--type Years = [L.ByteString]

--- csv2HMDIF , takes a .csvFile, defect, a  HMDIF header, a Carriage Way and the .csv file
csv2HMDIF_aux :: FilePath -> -- path to the .csv file
             FilePath -> -- path to write the dreated .hmd file to
             Defect   -> -- the defect to calculate
             String   -> -- the date to begin from
             IO (Maybe String)
csv2HMDIF_aux fileName outPathFile defect date = do
    -- get the header
    let headerFile =  "./SimulationFiles/hmdifHeader.txt"
    exists <- doesFileExist  headerFile
    if (not exists) then
        return $ Just "The HMDIF header file is missing. Cannot create HMDIF file."
    else do
        header <- L.readFile headerFile
        --let bs2Double2 = Just . bs2Double
        let missing  = Just 999.0
        let getData =  map (sequence . filter (/= missing) .  map (bs2Double1 . L.filter (not. isSpace)) . L.split ',')
        mdata <- liftM (sequence . getData . L.lines) $ L.readFile fileName
        --print ("file path is: " ++ fileName )
        -- mdata <- liftM (Just . map (map (read . L.unpack))) $ bsReadChr  ',' fileName
        case mdata of
            Nothing -> return (Just "Error reading .csv file")
            Just  xs  ->
                case getDate (L.pack date) of
                    (Nothing, str) -> return (Just str)
                    (Just year,_)  -> do
                        gen <- newStdGen
                        let rands        = map (/100.0) (randomRs (50.0, 150.0) gen :: [Double])
                        let valuesAsRows = transpose xs
                        let numChains    = fromIntegral $ length xs
                        let numYears     = maximum (map length xs)
                        let setYears     =  tail . dropWhile (/= '/') . tail . dropWhile (/= '/')
                        let inputYr      = read (setYears date) :: Int
                        let years        =  map (+ inputYr) [0 .. numYears]
                        let dateMiddle   =  takeWhile (/= '/') . tail $ dropWhile (/= '/')  date
                        let dateFront    =  takeWhile (/= '/') date
                        let datePrefix   = dateFront ++  dateMiddle
                        let bsYrs        =  map (L.pack . (datePrefix ++) . show ) years
                        let result1      = csv2HMDIFcreate valuesAsRows  secCode (Just secSorrg) (Just numChains)
                        let result2      = result1 (Just startTime) (Just endTime) Nothing Nothing
                        let (fFile,num)  = result2 bsYrs defect header rands -- cway
                        let hmEndRec     = L.cons '\n' (hmEnd <> bSlash <> (num2BS (num + 8)))
                        -- splitAtChunkBS :: L.ByteString -> L.ByteString -> (L.ByteString,L.ByteString)
                        let block        = L.pack ".csv"
                        let fname        = L.pack fileName
                        let outfile      = (L.unpack . fst $ splitAtChunkBS block fname) ++ ".hmd"
                        --let outPathFile  = "./SimulationFiles/hmdifPath.txt" -- hmdifPath.txt" ---
                        L.writeFile outfile (fFile <> hmEndRec)
                        L.writeFile outPathFile ((L.pack $ show defect) <> (L.cons ',' (L.pack outfile)))
                        print (show $ sum $ map length xs)
                        return Nothing
    where
        secCode     = 19023
        secSorrg    = 522440002
        startTime   = L.empty -- pack "23:00"
        endTime     = L.empty --pack "00:45"
        cway        = L.pack "CR1"
        deDta :: [Defect] -> [Double] -> [[Double]] -> [(Defect,[[Double]])]
        deDta ds rs xs
            | length ds < 2 = zip ds [xs]
            | otherwise     = zip ds [ziPP xs rs | _ <- ds ]
--SURVEY\TTS,,11,,TTS01,F,WDM,WDM;
--- and gernerates a .HMDIF file
csv2HMDIFcreate :: [[Double]] ->   -- data from the .csv file -- each row represents a year of data
                   Int -> -- need the section code
                   Maybe Integer -> -- need the section sorrogate (maybe integer)
                   Maybe Integer ->  -- the length of each section
                   --L.ByteString -> -- the start date of the survey
                   Maybe L.ByteString ->  -- start time of the survey
                   Maybe L.ByteString ->  -- end time of the survey
                   Maybe Int -> -- the stat of a chain . 0 if nothing
                   Maybe Int -> -- the lenght of each chain. 10 if nothing
                   -- need the section code , the start date and the year interval
                   [L.ByteString] ->        -- the years for which there is data
                   Defect ->       -- the type of defect
                   HMDheader ->    -- a HMDIF header (this is standard)
                   [Double] ->     -- a list of random integers between 1 and 0
                   -- CWay ->         -- the carriage way for which to calculate data
                   (L.ByteString,Int)    -- shouln't I return the data for the output file and the number of values?
csv2HMDIFcreate  values sCode mSorg mlen mStime mEtime mstart
                 mChainlength years defect hmdheader rands  =
                 (dataBlock,recordCount  )
    where
        ---
        surveyStr = "SURVEY\\TTS,,"++ show (length dataYrs) ++",,TTS01,F,WDM,WDM;\n"
        ---
        dataBlock = hmdheader <> (L.pack "\n") <> headerDelimiter <> (L.pack "\n") <>
                    (L.pack surveyStr) <>
                    (L.concat (map setYrsRec $ dataYrs)) <> footerDelimiter <>
                    bSlash <> num2BS recordCount <> semicolon
        recordCount = foldl' (\a (_,b) -> a + 1 + k * (length b)) 0 dataYrs
            where k = maybe 2 (\_ -> 4) $ listToMaybe rands
        ---
        dataYrs = zip years values
        --
        obsv    = observDelimiter <> bSlash
        mkov' st end df cw = L.intercalate comma [ obsv <> df, cw , -- carriageWay ,
                                            L.pack (show st) , L.pack (show end)]
        mkov a b df cw =  (mkov' a b df cw) <> (L.pack ";\n")
        --- onval
        mkOvl' val  = L.intercalate comma [obval 13,L.empty,num2BS val,L.pack "V"]
        mkOvl val   =  (mkOvl' val) <> (L.pack ";\n")
        -- [ov] [cv
        --
        mkObVal st ln df vals =  map mkDdats $ (zip cws   cr1Data ) -- ++ cl1Data))
            where
                mkDdats (cw, (v,ch)) = (mkov ch  (ch + ln) df cw) <> (mkOvl v)
                cr1Data  = zip vals [st,ln .. ]
                --cl1Data  = zip (zipWith (*) vals rands) [st,ln .. ]
                cr1      =  L.pack "CR1"
                --cl1      =  L.pack "CL1"
                len      =  length vals
                cws      =  replicate len cr1  -- ++ (replicate len cl1)

        -- SECTION\40070,529207007,1080,OBSERV\LLRT,CR1,10.0,20.
        --vals    =  map (map num2BS) values
        setYrsRec  (year, vals) =
            (mkSectionRec sCode mSorg len year mStime mEtime) <>
            (L.concat $ mkObVal startChain chainLength deft vals) -- <> (L.pack "\n")
        --
        obval n  =  obValDelimiter <> bSlash <> (L.pack $ show n)
        deft     =  L.pack (show defect)
        --cws      =  iterate id cr1 -- map L.pack ["CL1","CR1"]
        --cr1      =  L.pack "CR1"
        --
        startChain     = maybe 0 id mstart
        chainLength    = maybe 10 id mChainlength
        len            = maybe 10 id mlen
        --cStart  =
        --cEnd    =

mkSectionRec :: Int ->
                Maybe Integer ->
                Integer ->
                L.ByteString ->
                Maybe L.ByteString ->
                Maybe L.ByteString -> L.ByteString
mkSectionRec sCode mSorg len sdate mStime mEtime = sectionDelimiter <> bSlash <> (sectR'  <> (L.pack ";\n"))
    where
        etime = maybe L.empty id mEtime
        stime = maybe L.empty id mStime
        sorrg = maybe L.empty num2BS mSorg
        sectR' = L.intercalate comma [num2BS sCode, sorrg, num2BS len, sdate, sdate, stime,etime]

-- get year -- takes a bytsring like "mm/dd/yy" and returns the yy as an integer
-- and the date with the slahes removed as a string. If the date is invalid, the string
-- gives and appropriate error mesage
getDate :: L.ByteString -> (Maybe Int, String)
getDate bs
    | null ys        = (Nothing,invalid)
    | length ys == 3 =  maybe (Nothing, invalid) (\a -> (Just a, valid)) (bs2Int' yrv)
    | otherwise      = (Nothing, invalid)
                {--
                if L.length yrv == 2 then
                    if n >= 50 && n <=
                else if L.length  yrs == 4 then
                    if n >= 1980 && n <=
                else
                    (Nothing, invalid)
                --}
    where
        ys      = L.split '/' bs
        yrv     = ys !! 2
        valid   = L.unpack (L.filter (/= '/') yrv)
        invalid = "The value enterd is not a valid date"

ziPP :: Num a =>  [[a]] -> [a] -> [[a]]
ziPP  []     _  = []
ziPP (x:xs)  ys =  (zipWith (*)  x front) : ziPP xs back
    where
      (front,back) = splitAt (length x) ys
