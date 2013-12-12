
{- |
   This module contains preliminary functions and definitions for
   working with the HMDIF file format
-}
--
module HMDIFPrelude ((<>),(<!)
     ,Defect(LCOO,LSPD,LCRV,LFAL,LGRD,LLTX,LLTD,LLTM,LLTV,LCTM,
             LCTV,LRTM,LRTV,LT05,LT95,LTVV,LV3,LL03,LV10,LL10,LLBI,
             LR03,LR10,LRBI,LLRT,LLRD,LRRT,LRRD,LTAD,LTRV,LEDR,LES1,
             LES2,LEDC,LTRC,LWCL,LWCR,LECR,LRCR,LMAP,LSUR,LOVD,RCI,SFC)
     ,display
     ------
     ,typesAndDetails,getTypeFromDetail,conditions,conditionsCR
     ,bs2Double ,bs2Num,nthComma,nthSplit,nthM, bs2Int', bs2Double1
     ,bs2Integer , takeB4Block, dropB4Block
     -- splitting byte string
     ,splitAtChunkBS,splitAtChunksBS,splitAtChunkBSx,splitAtChunksBSx,removeBlock
     ,findBetweenBS,findBetweenBSx,subStringB,replaceAllBS,findChunkBSx -- splitAtChunksBSx',
     -- delimiters
     ,obValDelimiter,observDelimiter,thresholdDelimiter,surveyDelimiter
     ,sectionDelimiter,footerDelimiter,headerDelimiter,hmEnd,colon
     ,semicolon,comma,bSlash,tStart,tEnd,hmStart,fullStop,quote
     -- misc functions
     ,hmdifFooter,scode2align,string2SCODE
     ,aN,iN,fDot,num2BS,e2num,isNumBS,breakX,breakX3Plus,nthComma'
     ,spChks,bsReadChr
     , module L) where
--}
{------------------------------------------------------------------------------
Applications which received HMDIF files must:
    -   Be able to process leading and trailing spaces in all data items (except
        text strings, where there are considered to be part of the string). All
        other spaces must be maintained
    -   Handle files where the SECTION records are in any order
    -   Handle files where the OBSERV records within a SeCTION are in any order
    -   Be able to to accept and process additional observations collected on
        the same date but delivered in a different HMDIF import file
    -   Ignore records and data in the file which are inappropriate to the
        application. This requirement is needed to allow evolution of HMDIF file
        structures (i.e. the feeder system will be producing data required by as
        later version of the target applicaition) and the potential for an HMDIF
        output file to have more than one target application
------------------------------------------------------------------------------}
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (sortBy,groupBy,partition,unzip3, find, delete,foldl',nub, nubBy,deleteBy, findIndex)
import Data.Ord (comparing)
import Data.Char (isDigit, isAlpha,isSpace,isAlphaNum)
import Data.Maybe (listToMaybe, fromJust, isJust, maybe)
import Control.Monad (liftM,liftM2)
--
import ProcessFile (Display, display,Alignment(..)) -- ,bs2Double')
------------------------------------------------------------------------------

infixl 3 <>
infixr 1 <!
(<>)  =   L.append
(<!)  =   L.intercalate

-- ----------------------------------------------------------------------------------------------
-- everything before "DSTART;" is considered as the header - these are not procesHMDIFPrelude.hssed in any way
headerDelimiter,footerDelimiter,sectionDelimiter,surveyDelimiter,thresholdDelimiter,hmStart :: L.ByteString
headerDelimiter = L.pack "DSTART;"
-- similarly, everything after
footerDelimiter = L.pack "DEND"
--
sectionDelimiter = L.pack "SECTION"
--
surveyDelimiter = L.pack "SURVEY"
--
thresholdDelimiter = L.pack "THRESHLD"

---
observDelimiter,obValDelimiter,comma,bSlash,semicolon,tStart,tEnd,quote,fullStop,hmEnd :: L.ByteString
observDelimiter = L.pack "OBSERV"
--
obValDelimiter = L.pack "OBVAL"
semicolon  = L.cons ';' L.empty
colon  = L.cons ':' L.empty
comma      = L.cons ',' L.empty
bSlash     = L.cons '\\' L.empty
tStart     = L.pack "TSTART"
tEnd       = L.pack "TEND"
hmStart    = L.pack "HMSTART"
hmEnd      = L.pack "HMEND"
fullStop   = L.cons '.' L.empty
quote      = L.cons '"' L.empty


hmdifFooter = L.pack "\nDEND\\399009;\nHMEND\\399017;\n"

-- ----------------------------------------------------------------------------
-- | Replaces the element in position n with y. This function does not give
--   an out-of-bounds error
replaceAt n y xs | n <= 0 = xs
                 | otherwise =
                    let (front,back) = splitAt n xs  in
                        case back of
                        [] -> if length front == n
                                 then init front ++[y]
                                 else front
                        _  -> (init front) ++ (y:back)

-- | takes a comma-separated ByteString, interprets it as a comma separated
-- list, and returns the nth element nthSplit is a more general version of nthComma
nthSplit :: Int -> Char -> L.ByteString -> Maybe L.ByteString
nthSplit n  chr bs | null list         = Nothing
                   | n <= 0          = Nothing
                   | n > length list = Nothing
                   | otherwise       = Just . (!!(n-1)) $ list
              where list = L.split chr bs

-- projects the nth element in a comma-separeted bytestring
nthComma :: Int -> L.ByteString -> L.ByteString
nthComma  n  bs
    | L.null bs      = bs
    | otherwise       = (!!(n-1)) $ L.split ',' bs--  = maybe L.empty id  $ nthSplit n ','  bs

-- | ByteString repreenation of a number
num2BS :: (Num a ,Show a) => a -> L.ByteString
num2BS  = L.pack . show

-- | Converts a byteString to a Double
bs2Num :: (Read a, Num a) => L.ByteString -> a
bs2Num = read . L.unpack

--
bs2Double :: L.ByteString -> Double
bs2Double  = read . L.unpack


-- same as bs2Doule but deos not return an error message
bs2Double1 :: L.ByteString -> Maybe Double
bs2Double1 bs
    | L.null exx = liftM (negate *) $  bs2Double_aux rest
    | otherwise  =
        case finExp of
            Nothing  ->  Nothing
            Just n   ->
                if subStringB (L.pack "-") exx then
                    liftM ((/ (10^n)) . (negate *)) $  bs2Double_aux dde
                else
                    liftM ((* (10^n)) . (negate *)) $  bs2Double_aux dde
    where
        (neg, rest) = L.break isDigit bs
        (dde, exx)    = L.break (== 'e') bs
        neg'        = L.filter (not . isSpace) neg
        negate      = if L.null neg' then 1 else (-1)
        finExp      = bs2Integer (L.dropWhile (not . isDigit) exx)
        fn = fromIntegral
        --
        bs2Double_aux str =
            case L.readInteger str of
                Nothing -> Nothing
                Just (digit,rest) ->
                    if L.null rest then
                        Just $ fn digit
                    else
                        let (dot, trest) = (L.head rest, L.tail rest) in
                            if dot == '.' then
                                if L.length trest > 0 then
                                    if (not . isDigit . L.head) trest then
                                        Nothing
                                    else
                                        case L.readInteger trest of
                                            Nothing -> Nothing
                                            Just (rst,otr) ->
                                                if L.null otr then
                                                    Just (fn digit + (fn rst)  / 10**(fromIntegral $ L.length trest))
                                                else Nothing
                                else Nothing
                            else
                                Nothing
--}
--
bs2Int' :: L.ByteString -> Maybe Int
bs2Int' bs = liftM (* negate) $  bs2Int_aux rest
    where
        (neg, rest) = L.break isDigit bs
        neg'        = L.filter (not . isSpace) neg
        negate      = if L.null neg' then 1 else (-1)
        --
        bs2Int_aux str =
            case L.readInt str of
                Just (n,rest) ->
                    if L.null rest then Just n else Nothing
                _             -> Nothing
       
-- converts t bytestring to an Integer
bs2Integer :: L.ByteString -> Maybe Integer
bs2Integer bs = liftM (* negate) $  bs2Integer_aux rest
    where
        (neg, rest) = L.break isDigit bs
        neg'        = L.filter (not . isSpace) neg
        negate      = if L.null neg' then 1 else (-1)
        --
        bs2Integer_aux str =
            case L.readInteger str of
                Just (n,rest) ->
                    if L.null rest then Just n else Nothing
                _             -> Nothing


-- break a bytestring, keeping the character at which the string is broken
-- in the first string
breakX :: (Char -> Bool) -> L.ByteString -> (L.ByteString, L.ByteString)
breakX  p  bs | L.null back = (front, back)
              | otherwise =  (L.snoc front hd, L.tail back)
    where
        (front, back) = L.break p bs
        hd            = L.head back

-- breaks a bytestring in three or more parts by the given character
breakX3Plus  :: (Char -> Bool) -> L.ByteString ->
                Either (L.ByteString, L.ByteString, L.ByteString)
                       (L.ByteString, L.ByteString, L.ByteString, L.ByteString)
breakX3Plus p bs | L.null rest = Left (first, second, third)
                 | otherwise   = Right (first, second, third, rest)
    where
        (first, back) = breakX p bs
        (second, end) = breakX p back
        (third, rest) = breakX p end
--
nthComma' :: Int -> L.ByteString -> L.ByteString
nthComma'  n  bs   | L.null bs =  bs 
                   | otherwise = (!!(n-1)) . map (L.dropWhile (not . isAlphaNum)) $ L.split ',' bs
-- -----------------------------------------------------------------------------------------------
-- reading from files
-- bsReadChr,bsReadBlock,bsReadChr_aux,bsReadBlock_aux
bsReadChr   ::  Char -> FilePath -> IO ([[L.ByteString]])
bsReadChr   c =  liftM (bsReadChr_aux c)  .  L.readFile

bsReadBlock   :: L.ByteString -> FilePath -> IO ([[L.ByteString]])
bsReadBlock  c =  liftM (bsReadBlock_aux c) .  L.readFile

bsReadChr_aux :: Char -> L.ByteString -> [[L.ByteString]]
bsReadChr_aux   c   =  map (L.split c)  .  L.lines


bsReadBlock_aux :: L.ByteString -> L.ByteString -> [[L.ByteString]]
bsReadBlock_aux   block   =  map (splitAtChunksBS block) .  L.lines


-- taake and drop for blocks
takeB4Block :: L.ByteString  -> L.ByteString -> L.ByteString
takeB4Block    block   =  fst . splitAtChunkBSx block
--

dropB4Block :: L.ByteString  -> L.ByteString -> L.ByteString
dropB4Block    block   =  snd . splitAtChunkBSx block


{- --|
    splitAtChunkBS removes a substring in a bytestring, returning the
    bits preceeding and following the substring, respectively.
-}
splitAtChunkBS :: L.ByteString -> L.ByteString -> (L.ByteString,L.ByteString)
splitAtChunkBS  block  string | L.null block    = (string, L.empty)
                              | L.null string   = (L.empty, L.empty)
                              | found == block  = (front, back)
                              | L.null rest     = (front, rest)
                              | otherwise       =
                                   case splitAtChunkBS block  (L.tail rest) of
                                         (xx , yy) -> (front <> (L.cons hb xx)  , yy)
    where
        hb = L.head block
        (front, rest) = L.break (== hb) string
        (found, back) = L.splitAt (L.length block) rest

-- subString : Determines whether a string is a substring of a another
subStringB :: L.ByteString -> L.ByteString -> Bool
subStringB  block  string | L.null block && L.null string  = True
                          | L.null block   = False
                          | L.null rest    = False
                          | found == block = True
                          | otherwise      = subStringB block (L.tail rest)
    where
        hb = L.head block
        (_, rest)  = L.break (== hb) string
        (found, _) = L.splitAt (L.length block) rest


{- -| splitAtChunkBSx: jsut like splitAtChunkBS but the quired string prefixed to the latter list.
    In effect, splitAtChunkBSx splits a bytestrig at the beginning of a given string
-}
splitAtChunkBSx :: L.ByteString -> L.ByteString -> (L.ByteString,L.ByteString)
splitAtChunkBSx  block  string | L.null block    = (string, block)
                               | L.null string   = (string, string)
                               | found == block  = (front, rest)
                               | L.null rest     = (front, rest)
                               | otherwise       =
                                   case splitAtChunkBSx block  (L.tail rest) of
                                         (xx , yy) -> (front <> (L.cons hb xx)  , yy)
    where
        hb = L.head block
        (front, rest) = L.break (== hb) string
        (found, _ )   = L.splitAt (L.length block) rest

-- find a string preceeded by a certain string
findChunkBSx :: L.ByteString -> L.ByteString -> L.ByteString
findChunkBSx  block  string | L.null block    = block
                            | L.null string   = string
                            | found == block  = rest
                            | otherwise       = findChunkBSx block  (L.tail rest)
    where
        hb = L.head block
        (front, rest) = L.break (== hb) string
        (found, _ )   = L.splitAt (L.length block) rest

{- -|
    returns a list of all blocks separated by a given string. This function uses
    splitAtChunkBS so the quired block is removed from each element of the list.
    see splitAtChunksBSx
-}
splitAtChunksBS :: L.ByteString -> L.ByteString -> [L.ByteString]
splitAtChunksBS    block string | L.null block  = []
                                | L.null string = []
                                | L.null rest   = [] 
                                | otherwise     =  front : splitAtChunksBS block back
    where
        (_, rest) = splitAtChunkBS block string
         -- hd            = L.head rest
        (front, back) = splitAtChunkBSx block rest

{--|
    Just like splitAtChunksBS but the quired string is not removed from the elements of the list
-}

splitAtChunksBSx :: L.ByteString -> L.ByteString -> [L.ByteString]
splitAtChunksBSx    block string | found == block  = (found <> front) : splitAtChunksBSx block back
                                 | otherwise       = []
    where
        finBlock        = L.splitAt (L.length block) . snd .  splitAtChunkBSx block
        (found, rest )  = finBlock string
        (front, back)   = splitAtChunkBSx block rest -- (L.tail rest)


{- -|
    findBetweenBS: search between two given substrings. Returns the bits before, between
    and after the given bytestring
-}
findBetweenBS :: L.ByteString ->
                 L.ByteString -> L.ByteString -> (L.ByteString, L.ByteString, L.ByteString)
findBetweenBS  start end string = (first, middle,last)
    where  (first, rest ) = splitAtChunkBS start string
           (middle, last) = splitAtChunkBS end rest

-- just like findBetweenBS but 'start' is appended to the back of 'first' and
-- 'end' to the front of 'last', respectively. Need a better implenemtation of this
findBetweenBSx :: L.ByteString ->
                  L.ByteString -> L.ByteString -> (L.ByteString, L.ByteString, L.ByteString)
findBetweenBSx  start end string = (first, middle, last)
    where  (first, rest ) = splitAtChunkBSx start string
           (middle, last) = splitAtChunkBSx end rest

-- removes a bS occurring at the front of another BS
removePrefix :: L.ByteString ->L.ByteString -> L.ByteString
removePrefix  bs   bs1 | L.null bs  || L.null bs1 = bs1
                       | L.head bs == L.head bs1  = removePrefix (L.tail bs) (L.tail bs1)
                       | otherwise  =  bs1

-- removes a block from a BS
removeBlock blk bstring  = ft <> bk
    where
        (ft,bk) = splitAtChunkBS blk bstring

-- replace strings in a byteString
replaceBS :: L.ByteString ->L.ByteString -> L.ByteString -> L.ByteString
replaceBS  chr rchr  bs  | L.null rest = front
                         | otherwise   = front <> (rchr <> (removePrefix chr rest))
     where (front, rest) = splitAtChunkBSx chr bs

-- replaceAtBs: replaces the nth occurrence of a substring
replaceAtBS :: Int -> L.ByteString ->L.ByteString -> L.ByteString -> L.ByteString
replaceAtBS n bs rchr str
            | n <= 0      = str
            | L.null rest = front
            | n == 1      = front <> (rchr <> (removePrefix bs rest))
            | otherwise   = front <> bs <> ft <> (replaceAtBS (n - 1) bs rchr back)
    where
        (front, rest) = splitAtChunkBSx bs str
        (ft , back)   = splitAtChunkBSx bs (removePrefix bs rest)

-- replces before  the  nth occurrence of a bytestrgin
replaceBeforeN :: Int -> L.ByteString ->L.ByteString -> L.ByteString -> L.ByteString
replaceBeforeN n bs rchr str
            | n <= 0      = str
            | L.null rest = front
            | n == 1      = rchr <> rest
            | otherwise   = front <> bs <> (replaceBeforeN (n - 1) bs rchr  (removePrefix bs rest))
    where
        (front, rest) = splitAtChunkBSx bs str
        (_ , back)   = splitAtChunkBSx bs (removePrefix bs rest)

-- replces the nth occurrence of a bytestrgin
replaceAfterN :: Int -> L.ByteString ->L.ByteString -> L.ByteString -> L.ByteString
replaceAfterN n bs rchr str
            | n <= 0      = str
            | L.null rest = front
            | n == 1      = front <> bs <> (rchr <> back)
            | otherwise   = front <> bs <> (replaceAfterN (n - 1) bs rchr  (removePrefix bs rest))
    where
        (front, rest) = splitAtChunkBSx bs str
        (_ , back)    = splitAtChunkBSx bs (removePrefix bs rest)

--- returns values after the nth occurence of a BS. This function is
-- intended to take a list of items, given as a bytesstring (i.e.
-- "1,2,3,4,5" or [1,2,3,4,5]
afterNth :: Int -> L.ByteString -> L.ByteString -> L.ByteString -> L.ByteString
afterNth  n   par bf str
        | n <= 0      = str
        -- | L.null rest = front
        | n == 1      = ft
        | otherwise   = afterNth (n-1) par bf rest -- (removePrefix par rest)
    where (front,rest) = splitAtChunkBS par str
          (ft, _)      = splitAtChunkBS bf front


---
afterNth' :: Int -> L.ByteString -> L.ByteString -> L.ByteString -> Maybe L.ByteString
afterNth'  n   par bf str
        | L.null str  = Nothing
        | n <= 0      = Nothing
        -- | L.null rest = front
        | n == 1      = Just ft
        | otherwise   =
            maybe Nothing Just $ afterNth' (n-1) par bf rest 
    where (front,rest) = splitAtChunkBS par str
          (ft, _)      = splitAtChunkBS bf front


nthM :: Int -> L.ByteString -> Maybe L.ByteString
nthM   n  bs  = liftM remD $ afterNth' n comma semicolon bs
    where
        remD xs =  let (as, bs) = splitAtChunkBS bSlash xs in
                      if L.null bs then as else bs

-- replaces the substring between two bytestrng
replaceBetween start end rep string = a <> rep <> c
    where (a,_,c) = findBetweenBS start end string


--replaceAllBS :: Char -> Char -> L.ByteString -> L.ByteString
replaceAllBS chr rchr bs
        | L.null back = front
        | otherwise   = front <> rchr <>  (replaceAllBS chr rchr $ removePrefix chr back)
        --L.concat $ map (replaceBS chr rchr) bss
    where
        --cbs = L.cons chr L.empty
        (front, back) = splitAtChunkBSx chr bs

----------------------------------------------------------------------------------------------------
--      DATA TYPES FOR STORING HMDIF FILES
----------------------------------------------------------------------------------------------------
-- Survey type codes
surveyTypeCodes :: [String]
surveyTypeCodes = []


--isNumBS -- determines is a bytestring represents an integer of decimal number
isNumBS :: L.ByteString -> Bool
isNumBS bs = isNumBS_aux bs || (isNumBS_aux nm && isNumBS_aux dec )
    where
        isNumBS_aux bs | L.null bs = False
                       | otherwise = L.foldl (\c b -> c && isDigit b) True  bs
        (nm,dec) = splitAtChunkBS fullStop bs


-- HMDIF-specific numbers and text
-- iN - n-digit integers
--iN :: Int -> L.ByteString -> Bool
iN n bs | tt &&  m <= n = bs2Int' bs
        | otherwise    = Nothing
  where (tt, m) | L.null bs = (False, 0)
                | otherwise = L.foldl (\(a,n)  b -> (a && isDigit b, n+1)) (True,0) bs

-- fDot n d - indicates a real number field with up to n caracters, including the
--            decimal, with d digits after ther decimal point
--fDot ::  Int -> Int -> L.ByteString -> Bool
fDot n d bs | len  n d  && digit nm && digit dec =  Just $ bs2Double bs
            | otherwise = Nothing
    where
        len  nn dd    = let ld = L.length dec in
                          ld > 0 && ld <= dd &&  (nn-1) >= L.length nm + ld
        (nm,dec) = splitAtChunkBS fullStop bs
        digit = dig
        -- nDig  = hg
        dig   = L.foldl ( \a b ->   a && isDigit b) True

-- aN - a text field with n characters
aN n bs | m > 0 && tt && m <= n = Just bs
        | otherwise    = Nothing
    where (tt, m) | L.null bs = (False,0)
                  | otherwise = L.foldl (\(a,n)  b -> (a && isAlpha b || isDigit b, n+1)) (True,0) bs

-- e2num - takes a bytestring like m.ne-k and returns a bytestring like 0.0^(k-1)mn
e2num :: L.ByteString -> L.ByteString
e2num bs | allD fr    = maybe bs id (makeNm e k) -- $ Just (splitAtChunkBS (L.pack "-") bk)
         | otherwise  = bs
    where
        (fr,bk)  =  splitAtChunkBS fullStop bs
        makeNm be kk | L.length be < 2 = Nothing
                     | L.last be == 'e' && allD kk && (allD $ L.init be) =
                           case bs2Int' kk of
                             Nothing -> Nothing
                             Just n  ->
                                let m  = fromIntegral $ L.length fr
                                    ff = L.pack ("0."++(replicate (n-m) '0'))
                                    it = (L.init be)
                                    ed = if (L.length it == 1 && L.last it == '0') then L.empty else it
                                in
                                    Just (ff <> fr <> it)
                     | otherwise = Nothing
        (e,k)  =  splitAtChunkBS (L.pack "-") bk
        allD bb | L.null bb = False
                | otherwise = L.foldl (\a  b -> a && isDigit b) True bb
------------------------------------------------------------------------------------
-- | isFileName - determine is a bytestring represents a file name
isFileName :: L.ByteString -> Bool
isFileName str | L.null bk = False
               | L.null fr = isFileName bk
               | otherwise = valF fr && valB bk && L.length bk == 3
    where
        valB  = L.foldl (\a b -> a && isAlpha b) True
        valF  = L.foldl (\a b -> a && (isAlpha b || b == '/' || isDigit b))  True
        (fr,bk)  =  splitAtChunkBS fullStop str

------------------------------------------------------------------------------------------------
interL :: [(a,[b])] -> [(a,[b])]
interL    []          = []
interL  ((x,xs) : ys) = map (\a -> (x,[a])) xs ++ interL ys

interL'  = foldl (\as (x,xs) -> as ++ map (\a -> (x,[a])) xs) []
-------- testing

-- testbetween :: does not yet work as required
testBetween a b c d = L.unpack $ replaceBetween (L.pack a) (L.pack b) (L.pack c) (L.pack d)

isNum = isNumBS . L.pack

spChks a b = map L.unpack $ splitAtChunksBSx (L.pack a) (L.pack b)

remPrefix a b = L.unpack $ removePrefix (L.pack a) (L.pack b)

replaceS a b c = L.unpack $ replaceBS (L.pack a) (L.pack b) (L.pack c)

replaceAS n a b c = L.unpack $ replaceAfterN n (L.pack a) (L.pack b) (L.pack c)

afterN n a b c = L.unpack $ afterNth n  (L.pack a) (L.pack b) (L.pack c)
afterN' n a   =  liftM L.unpack $ nthM n  (L.pack a) -- (L.pack b) (L.pack c)

findBlockStr bl xs =
    let (as,ys) =  splitAtChunkBSx (L.pack bl) (L.pack xs) in (L.unpack as,  L.unpack ys)

findBetweenBStest a b c =
    let (x,y,z) = findBetweenBSx (L.pack a) (L.pack b) (L.pack c)
    in  (L.unpack x, L.unpack y, L.unpack z)
-------------------------------------------------------------------------------------------------
--  SCANNER CODES
-------------------------------------------------------------------------------------------------


---
string2SCODE :: String -> Maybe Defect
string2SCODE str = find ((== str). show) conditions

-- the condition profiles
conditions = (concatMap snd typesAndDetails) ++ [SFC]

conditionsCR = concatMap (\a -> [show a ++ "-CR1" , show a ++ "-CL1"]) conditions
-----
typesAndDetails =
    [("Texture",[LLTX,LLTD, LLTM, LLTV,LCTM,LCTV,LRTM,LRTV,LT05,LT95,LTVV]),
     --
     ("Rutting",[LLRT,LLRD,LRRT,LRRD,LTAD,LTRV]),
     --
     ("Cracking",[LTRC,LWCL,LWCR,LECR,LRCR,LMAP,LSUR,LOVD]),
     --
     ("Profile",[LV3,LL03,LV10,LL10,LLBI,LR03,LR10,LRBI]),
     --
     ("Edge Condition",[LEDR,LES1,LES2,LEDC]),
     ("Road Geometry",[LCRV,LFAL,LGRD]),
     ("Road Condition Indicator (RCI)",[RCI] )
    ]
--
getTypeFromDetail :: Defect -> String
getTypeFromDetail scode =
    case find ((elem scode) . snd)  typesAndDetails of
        Nothing       -> ""
        Just (str,_)  -> str

--
--- the fulction to map an alignmet to a SCHOSE
scode2align :: Defect -> Alignment
scode2align  sc | sc `elem` (scodes !! 3) = Avg
                | otherwise               = Max
    where scodes = map snd typesAndDetails

-- codes
data Defect =
    LCOO --        SCANNER or TTS Coordinate
-- Survey Speed
    | LSPD  --       SCANNER Speed
--  Road Geometry
    | LCRV  --       SCANNER or TTS Curvature
    | LFAL  --       SCANNER or TTS Crossfall
    | LGRD  --      SCANNER or TTS Gradient
-- Texture
    | LLTX  --      SCANNER or TTS Left Wheel Path Average Texture Depth (SMTD)
    | LLTD  --      SCANNER or Left Wheel Path  Average Texture Depth (MPD)
    | LLTM --
    | LLTV
    | LCTM  --      SCANNER Centre Mean RMST Texture Depth
    | LCTV  --      SCANNER Centre RMST Variance
    | LRTM  --      SCANNER Right Wheel Path Mean RMST Texture Depth
    | LRTV  --       SCANNER Right Wheel Path RMST Variance
    | LT05  --       SCANNER Overall Texture variability - RMST 5th Percentile Value
    | LT95 --       SCANNER Overall Texture variability - RMST 95th Percentile Value
    | LTVV  --       SCANNER Overall Texture Variability - RMST Variance
-- Longlitudinal Profile
    | LV3 --         SCANNER or TTS 3m moving average Longlitudinal Profile Variance (left/nearside)
    | LL03 --        SCANNER 3m enhanced Longlitudinal Profile Variance (left/nearside)
    | LV10 --        SCANNER 10m moving average Longlitudinal Profile Variance (left/nearside)
    | LL10 --        SCANNER 10m enhanced Longlitudinal Profile Variance (left/nearside)
    | LLBI --        SCANNER Bump Intensity (CDM) left wheel path
    | LR03 --        SCANNER 3m enhanced Longlitudinal Profile Variance (right/Offside)
    | LR10 --        SCANNER 10m enhanced Longlitudinal Profile Variance (right/offside)
    | LRBI --        SCANNER Bump Intensity (CDM) right wheel path
-- Transverse Profile
    | LLRT --        SCANNER or TTS Left Wheel Path Rut depth
    | LLRD --        SCANNER nearside rut depth from cleared profile
    | LRRT --        SCANNER or TTS Right Wheel Path Rut depth
    | LRRD --        SCANNER offside rut depth from cleared profile
    | LTAD --        SCANNER absolute deviation of 1st derivative of transverse profile
    | LTRV --        SCANNER transverse variance
-- Edge  condition
    | LEDR --        SCANNER edge roughness
    | LES1 --        SCANNER road edge step L1
    | LES2 --        SCANNER road edge step L2
    | LEDC --        SCANNER edge coverage
-- Cracking
    | LTRC --        SCANNER or TTS Cracking (whole carriageway)
    | LWCL --        SCANNER or TTS Left Wheel Track Cracking Intensity
    | LWCR --        SCANNER or TTS Right Wheel Track Cracking Intensity
    | LECR --        SCANNER Edge of Carriageway Cracking
    | LRCR --        SCANNER Transverse/reflection cracking
    | LMAP --        SCANNER of TTS crack map
    | LSUR --       SCANNER Surface Deteoration Parameter
    | LOVD --        SCANNER Other Visible Defect (OVD) Intensity
    --
    | RCI  --       Extracted RCI
    | SFC  --       SCRIM PArameter
    deriving (Eq, Show)

---
instance Display Defect where
    -- Location Referenceing
    display LCOO   =    "LCOO - SCANNER or TTS Coordinate"
-- Survey Speed
    display LSPD   =     "LSPD - Speed"
--  Road Geometry
    display LCRV   =     "LCRV - Curvature"
    display LFAL   =     "LFAL - Crossfall"
    display LGRD   =     "LGRD - Gradient"
-- Texture
    display LLTX   =     "LLTX - Left Wheel Path Avg. Texture Depth (SMTD)"
    display LLTD   =     "LLTD - Left Wheel Path  Avg. Texture Depth (MPD)"
    display LLTM   =     "LLTM - Left Weel Path Mean RMST Texture Depth"
    display LLTV   =     "LLTV - Left Weel Path  RMST Variance"
    display LCTM   =     "LCTM - Centre Mean RMST Texture Depth"
    display LCTV   =     "LCTV - Centre RMST Variance"
    display LRTM   =     "LRTM - Right Wheel Path Mean RMST Texture Depth"
    display LRTV   =     "LRTV - Right Wheel Path RMST Variance"
    display LT05   =     "LT05 - Overall Texture variability - RMST 5th percentile Value"
    display LT95   =     "LT95 - Overall Texture variability - RMST 95th perentile Value"
    display LTVV   =     "LTVV - Overall Texture Variability - RMST Variance"
-- Longlitudinal Profile
    display LV3    =     "LV3 - 3m moving avg. Longlitudinal Profile Variance (left/nearside)"
    display LL03   =     "LL03 - 3m enhanced Longlitudinal Profile Variance (left/nearside)"
    display LV10   =     "LV10 - 10m moving avg. Longlitudinal Profile Variance (left/nearside)"
    display LL10   =     "LL10 - 10m enhanced Longlitudinal Profile Variance (left/nearside)"
    display LLBI   =     "LLBI - Bump Intensity (CDM) left wheel path"
    display LR03   =     "LR03 - 3m enhanced Longlitudinal Profile Variance (right/Offside)"
    display LR10   =     "LR10 - 10m Enhanced Longlitudinal Profile Variance (right/offside)"
    display LRBI   =     "LRBI - Bump Intensity (CDM) right wheel path"
-- Transverse Profile
    display LLRT   =     "LLRT - Left Wheel Path Rut depth"
    display LLRD   =     "LLRD - Rut depth from cleared profile"
    display LRRT   =     "LRRT - Right Wheel Path Rut depth"
    display LRRD   =     "LRRD - Offside rut depth from cleared profile"
    display LTAD   =     "LTAD - Absolute deviation of 1st deriv. of transverse profile"
    display LTRV   =     "LTRV - Transverse variance"
-- Edge  condition
    display LEDR   =     "LEDR - Edge roughness"
    display LES1   =     "LES1 - Road edge step L1"
    display LES2   =     "LES2 - Road edge step L2"
    display LEDC   =     "LEDC - Edge coverage"
-- Cracking
    display LTRC   =     "LTRC - Cracking (whole carriageway)"
    display LWCL   =     "LWCL - Left Wheel Track Cracking Intensity"
    display LWCR   =     "LWCR - Right Wheel Track Cracking Intensity"
    display LECR   =     "LECR - Edge-of-Carriageway Cracking"
    display LRCR   =     "LRCR - Transverse/reflection Cracking"
    display LMAP   =     "LMAP - Crack map"
    display LSUR   =     "LSUR - Surface Deteoration Parameter"
    display LOVD   =     "LOVD - Other Visible Defect (OVD) Intensity"
    display RCI    =     "RCI  - SCANNER Road Condition Indicator"
    display SFC    =     "SFC  - SCRIM Parameter"
