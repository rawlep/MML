{-# LANGUAGE BangPatterns #-}
module ProcessFile (SMaybe (SNothing, SJust)
        ,DataFileType (Rutting,Texture,Profile,Cracking)
        ,Road (roadType, roadNames, roadDesp, sectionCodes)
        ,Alignment (Max, Min, Avg),evl
        , SchemeRec (..) ,  SchemeInRec (..)
        ----------------------------------------------------
        ,writeOrAppend,getSections,log2File
        ,Display, display,DL (To, Str,Onto,FromTo),displayList
        ----------------------------------
        ,readSecAndChainFile,readSegmentUIDfile,readRoadTypeSL,getRoadNamesAndSections,getRoadNames
        ,getLicense,writeToFile
        ,writeRoadsClass2File
        -----------------------------------
        ,bs2Double',schemeRec2String,schemeRec2Name--,readSchemeRec, createSchemeFile,
        ----------------------------------
        ,readRoadTypeS,getRoadClasses,readSchemeRec1,setRoadClasses
        ,splitBy -- string2Double,
        ,replaceAt, replace,replaceSnd,replaceSnd',replaceSnd1, replaceBy1,removeAt
        ,adjustValues,zipAdd,intercalate,partitions,intercalateFormList
        ,insertIndex,consIfBy
        ------------------------------------------
        ,seqMap, parMapM, parMapM_ , mapP
        ------------------------------------------
        ,rewriteEfloat, myTime,partitionBy
        ) where


import qualified Data.ByteString.Lazy.Char8 as L
import Time
import System.CPUTime
import Data.Time.Clock
import Data.Maybe (listToMaybe, fromJust, isJust)
import Control.Parallel.Strategies
import Control.Parallel (pseq)
import System.Directory (doesFileExist, doesDirectoryExist,createDirectory,
                         getPermissions,setPermissions,writable, readable)
import Data.List (sortBy,groupBy,partition,unzip3, find, delete,foldl',nub, nubBy,deleteBy,sort)
import Data.Ord (comparing)
import Data.Char (isDigit, isAlpha,isAlphaNum)
import Data.Maybe (listToMaybe,maybe)
import Data.IORef
import Control.Monad (liftM)
import IO
import GHC.IO.Handle (hClose)
-------------------------------------------------
import RprMix2 (Mmodel)
import LMisc (butLastElm,differentiateBy, myTake)
data SMaybe a = SNothing String | SJust a
-------------------------------------------------

--the number of unique elements
num_unique   []   = 0
num_unique (a:xs)| null ass  =  num_unique rest
                 | otherwise =  1 + num_unique rest
            where (ass, rest) = partition (==a) xs

-----------------------------------------------------------------------------------
---- miscellaneous functions
fill (x:y:xs) = [x,x+0.025 .. y] ++ fill(y:xs)
fill xs       = xs

---
zipAdd :: Num a => [a] -> [b] -> [(a,b)]
zipAdd  _   [] = []
zipAdd  [] _   = []
zipAdd [x]  (a:y:ys) = (x,a) : zipAdd [x+1] (y:ys)
zipAdd  (x:xs) (y:ys) = (x,y): zipAdd xs ys

-- combines map and filter: only applies f to predicates satisfying some condition
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f g = foldr (fg f g) []
    where fg f' g' x bs = if g' x then f' x : bs  else bs

-- insertIdx: takes a list and inserts the index of each element
insertIndex :: [a] -> [(a,Int)]
insertIndex = foldr (flip iDx) [] -- foldl'
--
iDx [] a             = [(a,0)]
iDx bs@((_,n): _) a  = (a,n+1):bs

-----------------------------------------------------------------------------------
-- adds an element to a list if it is not already in it
consIfBy :: (a -> a -> Bool) -> a  -> [a] -> [a]
consIfBy  f  x  xs = maybe (x:xs) (\_ -> xs) (find (f x) xs)
---------------------------------------------------------------------

-- replace a b xs: replaces the first occurance of a with b in the list xs
replace :: Eq a => a -> a -> [a] -> [a]
replace  = replaceBy1 (==)

replaceBy1 :: (b -> a -> Bool) -> b -> a -> [a] -> [a]
replaceBy1 f x y   = replaceGeneral f x (\_ -> y)
--
replaceGeneral :: (b -> a -> Bool) -> b -> (b -> a) -> [a] -> [a]
replaceGeneral  f  b g xs = let (front, back) = break (f b) xs in
                                case back of
                                  [] -> front
                                  ys@(_:qs) -> front++(g b : qs)

-----------------------------------------------------------------
replaceSnd :: Eq a => a -> [(b,a)] -> [(b,a)]
replaceSnd  a ((x,y):xs) | a == y    = (x,a) : xs
                         | otherwise = (x,y) : replaceSnd a xs
replaceSnd _ []  = []
----
replaceSnd' :: Eq a => a -> [(b,a,c)] -> [(b,a,c)]
replaceSnd'  a ((x,y,z):xs) | a == y    = (x,a,z) : xs
                            | otherwise = (x,y,z) : replaceSnd' a xs
replaceSnd' _ []  = []


replaceSnd1 a xs = replaceGeneral (\x y -> fst y == x) a (\a -> (a,xs))

-- replace a b xs: replaces all occurences a in xs with b
replaceAll:: Eq a => a -> a -> [a] -> [a]
replaceAll  x y xs =
        let (front, back) = break (== x) xs in
            case back of
            [] -> front
            ys@(_:qs) -> front++(y: replaceAll x y qs)


-- replaces the element in position n with y
-- This function does not give an out of bounds error
replaceAt n y xs | n <= 0 = xs
                 | otherwise =
                    let (front,back) = splitAt n xs  in
                        case back of
                        [] -> if length front == n then
                                init front ++[y]
                              else
                                front
                        _  -> case front of
                                [] -> back
                                _  -> (init front) ++ (y:back)

-- remove x xs: removes the first occurrence of x in the list xs
remove_first :: Eq a => a ->  [a] -> [a]
remove_first  x xs = delete x xs

--- removes the element at position n
removeAt ::Int -> [a] -> [a]
removeAt  n xs | n < 1 = xs
               | otherwise =
                  let (front, back) = splitAt (n-1) xs in
                   case back of
                    [] -> front
                    (_:qs) -> front ++ qs

--- removes m elements starting from  position n
removeNfrom :: Int -> Int -> [a] -> [a]
removeNfrom  m n xs | n < 1 = xs
                    | otherwise =
                       let (front, back) = splitAt (n-1) xs in
                          front ++ drop m back

--- place a value between emelents of the list
intercalate :: a -> [a] -> [a]
intercalate a [x,y]  = [x,a,y]
intercalate a (x:xs) =
    case intercalate a xs of
        (z : ys@(_ : _))  -> (x:a:z:ys)
        ks       ->  x:ks
intercalate _   xs    =  xs
--

intercalateFormList :: [a] -> [a] -> [a]
intercalateFormList  _  []            = []
intercalateFormList  [] xs            = xs
intercalateFormList  (a:as) xs@(b:bs)
            | null bs                 = xs
            | otherwise               =  b : a : intercalateFormList as bs


partitions ::  (a ->  Bool) -> [a] -> [[a]]
partitions  f [] = []
partitions  f xs = front : partitions f back
    where (front,back) = partition f xs

      | otherwise = 1 + expn (n `div` 10)

-- takes a ByteString like "23.67" and turns it into a float
bs2Double :: L.ByteString -> Double
bs2Double  =  read . L.unpack

-- same as bs2Doule but deos not return an error message
bs2Double' :: L.ByteString -> Maybe Double
bs2Double' str =
     case L.readInteger str of
      Nothing -> Nothing
      Just (digit,rest) ->
        if L.null rest then
            Just $ fn digit
        else
            let (dot, trest) = (L.head rest, L.tail rest) in
            if dot == '.' then
                case L.readInteger trest of
                    Nothing -> Nothing
                    Just (rst,otr) ->
                        if L.null otr then
                            Just (fn digit + (fn rst)  / 10**(fromIntegral $ L.length trest))
                        else Nothing
            else
                Nothing
            where fn = fromIntegral

-- takes a comma separated ByteString, interprets it as a comma
-- separated list, and returns the nth element
-- nthSplit is a more general version of nthComma
nthSplit :: Int -> Char -> L.ByteString -> L.ByteString
nthSplit n  chr bs | n <= 0 || n > length list = bs
                   | otherwise              =  (!!(n-1)) $ list
              where !list = L.split  chr bs


nthComma :: Int -> L.ByteString -> L.ByteString
nthComma  n  bs   | L.null bs =  bs
                  | otherwise = (!!(n-1)) $ L.split ',' bs
{----}
num2Bs ::Num a => a -> L.ByteString
num2Bs  = L.pack . show

-- takes a bytestring like mm/dd/yyrr and returns the value for yyrr
year :: L.ByteString -> Int
year str =
    case L.readInt str of
      Nothing ->  error "error extracting ByteString year value"
      Just (_,rest) ->
        case L.readInt (L.tail rest) of
          Nothing ->  error "error extracting ByteString year value"
          Just (_, rst) ->
            case L.readInt (L.tail rst) of
              Nothing ->  error "error extracting ByteString year value"
              Just (yr, _ ) -> yr

--- takes a bytrstring representation of an integer and returns its
--- value an integer
bs2Int :: L.ByteString -> Int
bs2Int str =
    case L.readInt str of
      Nothing -> error "error converting ByteString to Int"
      Just (num,_) -> num

--
bs2Int' :: L.ByteString -> Maybe Int
bs2Int' str =
    case L.readInt str of
      Nothing -> Nothing
      Just (num,rest) ->
         case L.readInt rest of
            Nothing -> Just num
            Just _ -> Nothing


----------------------------------------------------------------------------------------------------
class Display a where
    display :: a -> String

data DL = To | Str  String | Onto | FromTo

instance Display DL where
    display To = "      to      "
    display (Str str) = str
    display Onto = "    onto    "
    display FromTo = ""

--
displayList :: Show a => DL -> [a] -> String
displayList _ []   = ""
displayList _ [a]  = show a
displayList FromTo xs  = "From " ++ show (head xs) ++ " to " ++ show (last xs)
displayList dl xs  = show (head xs) ++ display dl ++ show (last xs)


----------------------------------------------------------------------------------------------------
--- The redfile function to use with the simulation data
---readFromFile :: FilePath -> IO ([[Double]])
--readFromFile filename   =
--    --do
--      inputStr <- L.readFile filename
--      let dds  =  map (map bs2Double . init. L.split ',') $ L.lines inputStr
--   ---print  dds
--      return dds

-- writes a string to a file
writeToFile :: FilePath -> String -> IO ()
writeToFile path str = do
    L.writeFile ("./mmlTdsFiles/"++path) $ L.pack str


--- this functin
readSecAndChainFile :: FilePath -> IO ([Int], [String],  [[String]])
readSecAndChainFile filename   =
    do
      inputStr <- L.readFile filename

      let getSecRefs xs =  (head xs,  head $ tail xs ,
                                      concatMap (map L.unpack . L.split ',') $ tail $ tail xs)
      let (segR, crr, chains)  = unzip3 .  map (getSecRefs . L.split '+') $ init $ L.lines inputStr
      --
      let chnStart = bs2Int . head . L.split '-'  . L.pack

      ---print  dds
      return (nub $ map bs2Int segR
              ,map (filter (\a -> isDigit a || isAlpha a )) $ map L.unpack crr
              ,map (sortBy (comparing chnStart) . nub) chains)

-- read from the file with section UIDs
readSegmentUIDfile :: FilePath -> IO [String]
readSegmentUIDfile filename   =
    do
      inputStr <- L.readFile filename
      let dds  =  map L.unpack $ L.lines inputStr
      return dds


--- gets the license file
getLicense :: FilePath -> IO String
getLicense filePath = do
    contents <- L.readFile filePath
    let license = L.unpack contents
    return license


---------------------------------------------------------------------------------------------------
makeWritable f = do
     p <- getPermissions f
     setPermissions f (p {writable = True})

makeReadable f = do
     p <- getPermissions f
     setPermissions f (p {readable = True})

makeReadWriteable f = do
     p <- getPermissions f
     setPermissions f (p {readable = True})
     setPermissions f (p {writable = True})

---------------------------------------------------------------------------------------------------
--
---------------------------------------------------------------------------------------------------

--- Road (roadType, roadNames, roadDesp, sectionCodes)
data Road = Road {
             roadType  :: String,             --- A, B, C, etc
             roadNames  :: [L.ByteString],    --- The alphaNumeric code of the road from roads.txt, e.g. Z1120
             roadDesp   ::  [[L.ByteString]], -- The english description of the road
             sectionCodes :: [[L.ByteString]] -- the SCANNER section code of the road
             -- sectionSorrg :: [[L.ByteString]] -- the SCANNER section sorrogate
             } deriving (Eq, Show)

---------------------------------------------------------------------------------------------------
--- wiriting all road names to sepearte files
--writeRoadsClasses2File :: Bool -> IO ()
writeRoadsClass2File :: Char -> (String -> IO()) -> Bool -> IO ()
writeRoadsClass2File rclass write rewrite = do
    write ("Extracting attributes for "++ (rclass : "Roads"))
    let sAtts  = "./roadAttributes/SECTION_ATTRIBUTES.txt"
    let iff a b tf = if tf then  a else b
    slist  <- L.readFile sAtts >>=  return . filter (not . L.null) . L.lines
    rlist  <- L.readFile "./roadAttributes/roads.txt" >>= return . filter (not . L.null) . L.lines
    let flush  = filter (((Just rclass) ==) . getC)
    let !pClasses   =  maybe  [] (:[]) $ find ((rclass ==) . L.head . fst) ((partitionByClass . flush) rlist)
    if null pClasses then do
        write ("Sorry, there are no " ++ (rclass : "roads. "))
    else do
        let  (codesGroup, rest) = (groupByCodes pClasses slist) 
        let len =  head $ map (length . snd) codesGroup
        --print (show $ map (length . snd) codesGroup)
        let insertNL = L.intercalate (L.pack "\n")
        L.writeFile "./roadAttributes/codes.txt" (insertNL $ map (insertNL . snd) pClasses)
        let numAtts = (show len) ++ " attributes found."
        write ("Finished extracting attributes for "++ (rclass : "roads. ") ++ numAtts)
        --hClose handle
        mapM_ (printCodes True) codesGroup

    where
        ---------------------------------------------------------------------------
        printCodes :: Bool -> (L.ByteString, [L.ByteString] ) -> IO ()
        printCodes rewrite (fName, dataForFile) = do
            let fileName = "./roadAttributes/" ++ (L.unpack fName)++".dat"
            if rewrite then
                L.writeFile fileName (L.intercalate (L.pack "\n") dataForFile)
            else do
                exist <- doesFileExist fileName
                if exist then
                    return ()
                else
                    L.writeFile fileName (L.intercalate (L.pack "\n") dataForFile)

        ---------------------------------------------------------------------------
        --- repeatedly preforms differentiateBy over a list of elements
        remO n = L.filter isAlphaNum . nthComma n
        getC bs | L.null (remO 2 bs)   = Nothing
                | otherwise            = Just (L.head $ remO 2 bs)
        --- groups all the section_attributes data by road class
        groupByCodes ::  [(L.ByteString, [L.ByteString] )] ->   -- a class and all the road sections in that class
                         [L.ByteString] ->  -- the list of section attributes
                         ([(L.ByteString, [L.ByteString] )] ,[L.ByteString ]) -- a class and its attributes, along with the remaining attributes
        groupByCodes       []     ys      = ([],ys)
        groupByCodes       xs     []      = (xs,[])
        groupByCodes ((a,!xs):xss) !ys    = ((a,zs) : want , rem)
            where
                (want, rem) =  groupByCodes xss rest
                (!zs, rest) = differentiateBy (remO 4) xs ys
        -- put group the roadcodes by road class
        partitionByClass :: [L.ByteString] -> [(L.ByteString, [L.ByteString] )]
        partitionByClass   []   =  []
        partitionByClass (x:xs) | null front = partitionByClass xs
                                | otherwise  = (name , codes) : partitionByClass back
            where
                (front, back) = partition (\a -> isJust (getC a) && getC a == getC x) xs
                Just cls = getC x
                name     = L.pack $ cls : "Roads"
                codes    = [nthComma 1 i | i <- front ]
---------------------------------------------------------------------------------------------------
---- dealing with schemes
--------------------------------------------------------------------------------------------------

data SchemeInRec = SchemeInRec {
    clsCode :: String,
    scnrCode  :: String,
    rdName   :: String,
    scSrg   :: String
    }

partitionBy :: (a -> a -> Bool) -> [a] -> [[a]]
partitionBy f xs
    | null xs   = []
    | otherwise = front : partitionBy f back
    where
        (y:_)       = xs
        (front,back) = partition (f y) xs

instance Show SchemeInRec where
    show (SchemeInRec a b c d) =  a ++ (' ' :  b)  ++ (' ' : c) ++   (' ' : d) -- "\n" ++

-- get the road classes
getRoadClasses1 :: [SchemeInRec] -> IO ()
getRoadClasses1 scm = do
    let scmList =  partitionBy (\a b -> compareStrHead (clsCode a) (clsCode b)) scm -- (scSrg a) == (scSrg b)
    let classes =  L.intercalate (L.pack "\n") . nub $ concatMap (map (L.pack . scSrg )) scmList
    L.writeFile "./roadAttributes/roadClasses.txt" classes
    where
        compareStrHead xs ys
            | null xs || null ys = False
            | otherwise          = (take 1 xs) == (take 1 ys)
--- settign the road classes
setRoadClasses wrt = readSchemeRec1 wrt >>=  getRoadClasses1


-- read the road classes from a file
readSchemeRec1:: (String -> IO ())  -> IO [SchemeInRec]
readSchemeRec1 prt =  do
    let roadNamesPath = "./SimulationFiles/roadNamesPath.txt"
    rexists <- doesFileExist roadNamesPath
    if rexists then do
        slist  <- L.readFile roadNamesPath >>=  return .  L.lines
        if null slist  then do
            prt "Error reading roadNames file at path. There will be limited filters for roads names and sections."
            return []
        else do
            let path = L.unpack . (!! 1) . (L.split ',') $ head slist
            --print path
            exists <- doesFileExist path
            if exists then do
                lists <- L.readFile path >>=  return .  map (L.split ',')  . L.lines . L.filter (/= '"')
                if not (null lists) then do
                    let pk = L.unpack
                    let mkScmRec [a,b,c,d] =  SchemeInRec (pk $  a) (pk b) (pk c) (pk d)
                    let mkXs xs = if length xs == 4 then Just (mkScmRec xs) else Nothing
                    let scms = map mkXs $ tail lists -- , isJust xs]
                    --case (sequence scms) of
                    return $ (map fromJust . filter isJust )  scms

                else do
                    prt "Error retrieving road names and classes. There will be limited options for SCANNER and SCRIM."
                    return []
            else do
                prt "The roadNames file at the current path does not exist. Need to update roadNames path..."
                return []
    else  do
        prt "No roadNames path. Need to update path..."
        return []
--}

-- a scheme record
data SchemeRec = SchemeRec {
    classsCode :: String,
    scanrCode  :: String,
    roadName   :: String,
    secSorrg   :: String,
    scmStart   ::  Int,
    scmEnd     ::  Int
    } deriving Eq

schemeRec2String :: SchemeRec -> String
schemeRec2String scr = -- concat $ intercalate "_" (fbit ++ map show lbit)
        "road class: "++ classsCode scr
        ++ "SCANNER code: " ++ scanrCode scr
        ++ "road name: " ++ roadName scr
        ++ "sorrogate: " ++ secSorrg scr
        ++ "start: " ++  show (scmStart scr)
        ++ "end: " ++ show (scmEnd scr)

---
schemeRec2Name :: SchemeRec -> String
schemeRec2Name scr = concat $ intercalate "_" (fbit ++ map show lbit)
    where
        fbit = [classsCode scr,  scanrCode scr , roadName scr ,  secSorrg scr]
        lbit = [scmStart scr, scmEnd scr]

--reads a scheme Rec file and returns the elements as a list of
schmRecFile2SchmRecs :: IO [SchemeRec]
schmRecFile2SchmRecs = do
    let schemeRecFile = "./LocalAnalysis/schemesRecFile.txt"
    fileExist <-  doesFileExist  schemeRecFile
    if fileExist then do
        slist  <- L.readFile schemeRecFile >>=  return . L.lines
        let scmRecs =  [out | scmrec <- slist
                           , let nthCommaS n = L.unpack . nthComma n
                           , let cCode = nthCommaS 1 scmrec
                           , let sCode = nthCommaS 2 scmrec
                           , let rName = nthCommaS 3 scmrec
                           , let sorrg = nthCommaS 4 scmrec
                           , let start = bs2Int' $ nthComma 5 scmrec
                           , let end   = bs2Int' $ nthComma 6 scmrec
                           , let out   = (cCode, sCode, rName, sorrg, start, end)
                           ]
        return [ SchemeRec a b c d ee ff | (a,b,c,d,e,f ) <-  scmRecs
                                          , isJust e && isJust f
                                          , let Just ee = e
                                          , let Just ff =  f
               ]
    else
        return []


--readRoadType
readRoadType :: [L.ByteString] -> IO Road
readRoadType  rlist' = do
    --roadRecs <- L.readFile "./roadAttributes/roads.txt"
    slist  <- L.readFile "./roadAttributes/SECTION_ATTRIBUTES.txt" >>=  return . L.lines
    let rlist = filter (not . L.null) rlist'
    let rtype = if null rlist then Nothing else Just (L.head $ head rlist)
    --
    let roadIdsNm = [ (rnm,xs) |  rls <- rlist,
                        let remO n = L.filter isAlphaNum . nthComma n,
                        --let roadName =  remO 3, -- L.init . L.tail . nthComma 3,
                        not (L.null rls),let rnm = remO 3 rls, not (L.null rnm),
                        --, L.head rnm == rtype,
                        let xs = [(remO 3 sids, remO 2 sids) | sids <- slist,
                                        let rId =  nthComma 1 rls,
                                        let srId = nthComma 4 sids,
                                        --let mkInt = bs2Int . nthComma 1,
                                        rId == srId]
                     ]
    let (names,rest) = unzip roadIdsNm
    let ( sectCodes, rIds) = unzip $ map unzip rest
    let rtype' = maybe [] (:[]) rtype
    return $ Road rtype' names rIds sectCodes

getRoadClasses :: IO [(L.ByteString , Char)]
getRoadClasses = do
    rlist  <- L.readFile "./roadAttributes/roads.txt" >>= return . filter (not . L.null) . L.lines
    return  $ sortBy (comparing snd)  [(nthComma 1 bs, cls (remO 2 bs) ) | bs <- rlist,
                                L.any (== ',') bs, let remO n = L.filter isAlphaNum . nthComma n,
                                let cls b  = if L.null b then '!' else L.head  b ]

getRoadNames :: IO [L.ByteString]
getRoadNames = do
    slist  <- L.readFile "./roadAttributes/SECTION_ATTRIBUTES.txt" >>=  return .  L.lines
    let remO n = L.filter isAlphaNum . nthComma n
    let !output = [ remO 3 bs  | bs <- slist]
    return output

-- road names and section codes
getRoadNamesAndSections :: [L.ByteString] -> IO [(L.ByteString,L.ByteString,L.ByteString)]
getRoadNamesAndSections codes = do
    slist  <- L.readFile "./roadAttributes/SECTION_ATTRIBUTES.txt" >>=  return .  L.lines
    let remO n = L.filter isAlphaNum . nthComma n
    let !output = map (\bs -> (remO 4 bs, remO 2 bs,remO 3 bs )) slist
    return ((inCodes codes [] output) `using` rseq)
        where
            inCodes [] !acc bss = acc
            inCodes _  !acc []  = acc
            inCodes xs@(a:as) acc (b@(x,_,_):bs)
                | a == x    = inCodes as (b:acc) bs
                | otherwise = inCodes xs acc bs

readRoadTypeS :: IO [Road]
readRoadTypeS = readRoadTypeS_aux ['A' .. 'Z']

readRoadTypeS_aux types = do
    rlist  <- L.readFile "./roadAttributes/roads.txt" >>= return . filter (not . L.null) . L.lines
    mapM readRoadType $ charsList rlist types  -- liftM (filter rNull) .
        where 
            charsList ys []       = []
            charsList ys (typ:xs) | null  front  =  charsList back xs
                                  | otherwise    =  front : charsList back xs
                where
                   (front,back) = partition (\a -> typ ==  remO (fl 3 a)) ys
                   fl n = L.filter isAlphaNum . nthComma n
                   remO bs | L.null bs = '!'
                           | otherwise = L.head bs
--
readRoadTypeSL c = do --readRoadTypeS_aux [c]
    rlist  <- L.readFile "./roadAttributes/roads.txt" >>=
                    return . filter (\a -> c ==  remO (fl 3 a)) . L.lines
    readRoadType rlist
    where
        fl n = L.filter isAlphaNum . nthComma n
        remO bs | L.null bs = '!'
                | otherwise = L.head bs


-- =================================================================================================
--
data DataFileType = Rutting | Texture | Profile | Cracking  deriving (Eq, Show)

--- the sort of alignment needed to be performed on the data when extracting it from the file
data Alignment = Max | Min | Avg deriving (Eq,Show)
instance Display Alignment where
    display Max = "Maximum"
    display Min = "Minimum"
    display Avg = "Average"
evl :: Alignment -> Double -> Double -> Double
evl Max = max
evl Min = min
evl Avg = \x y -> (x + y)/2


--splitBy: collect chunks of a list that satisfy some condition
-- e.g. splitBy even [1,2,4,6,5,8,12] = [[2,4,6],[8.12]]
splitBy _ [] = []
splitBy f xs | null front =  splitBy f (dropWhile (not . f) rest)
             | otherwise  =  front : splitBy f (dropWhile (not . f) rest)
    where (!front, !rest) = span f xs

----
adjustValues vals years = unzip $ fitDummy (zip vals years) (fillConsec years)
    where
        fitDummy :: (Num a , Eq b) => [(a,b)] -> [b] -> [(a,b)]
        fitDummy  _   []   = []
        fitDummy [] (x:xs) = (-10,x) : fitDummy [] xs
        fitDummy ((!val,!vyr):vs) (yr:yrs) | vyr == yr = (val,yr) : fitDummy vs yrs
                                           | otherwise = (-10,yr) : fitDummy ((val,vyr):vs) yrs
        fillConsec (!x:ys@(!y:ys')) | y-x == 1 = x:  fillConsec ys
                                    | otherwise = x : fillConsec ((x+1):ys)
        fillConsec  xs = xs

----
-- converts a floting point given as 8.2233e-k to 0.0^{k-1}82233
rewriteEfloat :: Int -> String -> String
rewriteEfloat n xs | null num =  let (fr, bk) = round xs in  fr ++ ('.' : roundNum n (tail bk))
                   | otherwise =
                      let m = read (myTake 1 num) :: Int
                          (fr, bk) = round xs
                      in  "0." ++  replicate (min (m-1) (n-1)) '0' ++ fr ++ roundNum (n- m) (tail bk)
    where
        (front, back) = break (== 'e') xs
        num = drop 2 back
        round  str  =  break (== '.') str
        roundNum :: Int -> String -> String
        roundNum  n   xs | n < 0 = []
                         | length bit == n + 1 = -- bit
                             if lNum >= 5 then
                                 myTake (n-1) bit ++ show k
                             else   init bit
                         | otherwise  =  bit -- init bit
            where bit = myTake (n+1) xs
                  k = 1 + (read [bit !! (n-1)] :: Int)
                  lNum     = read [last bit] :: Int

-------------------------------------------------------------------------------------------------
--      FILE ACCESS
-------------------------------------------------------------------------------------------------
log2File :: String -> String -> IO ()
log2File logfileName str = do
-- check if the logfile exists
    let logDir = "./Logs/"
    let logFile = logDir++logfileName++".log"
    --
    date <-   fmap show getClockTime -- liftM ( (!! 3) . words) $
    let string = L.snoc (L.pack (date ++": "++str)) '\n'
    -- create the log dir it does not exist
    doesDirectoryExist logDir >>= \yn -> if yn then return () else createDirectory logDir
    writeOrAppend logFile string

writeOrAppend fileName string =
    doesFileExist fileName >>= \yn -> do
        if yn then
            L.appendFile fileName string
        else
            L.writeFile fileName string

-- read the aligned data-points from the file
getSections :: FilePath -> IO ([[[Double]] ])
getSections fileName = do
    let getBSlist =  map (map (L.split ',' )) . map (L.split ';')
    let mkDoubles =  mapP (map fromJust . filter isJust . map bs2Double') --)
    L.readFile fileName >>= return .  map mkDoubles . getBSlist . L.lines
-----------------------------------------------------------------------------------------------
-- PARALLEL DEFINITIONS
-----------------------------------------------------------------------------------------------
seqMap f xs =  (map f xs) `using` evalList rseq
parMapM f = sequence . seqMap f
parMapM_ f = sequence_ . seqMap f
mapP = parMap rseq


-- timeing a computation; output the time it took to run
myTime :: IO t -> IO (t, String)
myTime a = do
    start <- getCPUTime
    !v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    -- printf "Computation time: %0.3f sec\n" (diff :: Double)
    return (v, show diff)