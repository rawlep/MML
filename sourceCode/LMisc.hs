{-# LANGUAGE BangPatterns #-}
module LMisc (zipp, zipp4, tokens, tokens', in2out, rDoubleS
              , findWithRemainder ,differentiateBy,findWithAdjs
              , takeFirst, mtone3, subs, subsN, choose, force
              ,ascending,nppP 
              , allPieces, nPieces, adjustMatrix , myMin
              , nPieces2,nPiecesPOpt, trendLine , predict, nPiecesP
              , butLastElm,maintainBy,myTake,maintainBy',sumBy,similarBy
              , fillZeroes,fillZeroes_aux, takeFirstM, oSubs
              , zipp3, myMin',zipMaybe,allPositiveSlopes , findIndexesPresent, maintainByLen
              , findCuts,mkIntervals,getOppFuns,printElm,printElmChr,predsFromLP
              ,  manyInvNS,findGrps
              , lastN, mapSnd
              )
              where
--- some miscellaneous list operations
import qualified Control.Parallel.Strategies as St ---(parList, rseq)
import IO
import Data.List (transpose, delete,findIndex,foldl',nub,maximumBy,minimumBy,find)
import Data.Ord (comparing)
import Data.Maybe (listToMaybe)
import Control.Parallel (par, pseq)
import Numeric.LinearAlgebra (fromList, toList, join, Vector, foldVector,takesV,dim,(@>),mapVector )
import Numeric.GSL.Statistics (mean,stddev)


----------------------------------------------------------------------------------------------------
--- create pairs of consecutive members of a lists
----------------------------------------------------------------------------------------------------
zipp :: [a] ->[(a,a)]
zipp     (x:y:xs)       =  (x,y):zipp (y:xs)
zipp       _            =   []

--
unZipp :: [(a,a)] -> [a]
unZipp [(a,b)]       = [a,b]
unZipp ((a,_) : xs)  = a : unZipp xs
unZipp   _           =  []

--
zipp3 ::  [a] -> [(a,a,a)]
zipp3  (a:b:c:xs)   = (a,b,c): zipp3 (b:c:xs)
zipp3      _        = []

--
zipp4 ::  [a] -> [(a,a,a,a)]
zipp4  (a:b:c:d:xs)   = (a,b,c,d): zipp4 (c:d:xs)
zipp4      _          = []

------------------------------------------------------------
--                                                        --
zipMaybe :: [a] ->[b] ->[(Maybe a, Maybe b)]
zipMaybe     []     []    =       []
zipMaybe   (a:as)   []    = (Just a, Nothing) : zipMaybe as []
zipMaybe     []    (b:bs) = (Nothing, Just b) : zipMaybe [] bs
zipMaybe   (a:as)  (b:bs) = (Just a, Just b)  : zipMaybe as bs
------------------------------------------------------------

 --mapSnd
mapSnd :: ([a],b) -> [(a,b)]
mapSnd  (xs,y) = St.parMap St.rseq  (\a -> (a,y)) xs

-- find the number of unique elements in a list
num_unique   []   = 0
num_unique (a:xs)| null ass  =  num_unique rest
                 | otherwise =  1 + num_unique rest
            where (ass, rest) = span (==a) xs

---- LastN : the dual of take n
lastN :: Int -> [a] -> [a]
lastN n xs  =  drop (m-n) xs
    where m = length xs



-- myTake - a strict version of take
myTake  _    []  =  []
myTake  0    _   =  []
myTake  n (x:xs) =  ys `seq` (x:ys)
    where     ys =  myTake (n-1) xs

--- summing using a function
sumBy :: Num a => (b -> a) -> [b] -> a
sumBy f = foldl' (\b a -> b + f a) 0


--- force a list down to the last term
force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1

--- finds an element of a list and returns the element along with
-- the remainder of the list
findWithRemainder :: (a -> Bool) -> [a] -> (Maybe a, [a])
findWithRemainder  f  []  = (Nothing, [])
findWithRemainder  f (x:xs)
    | f x        =  (Just x , xs)
    | otherwise  =
        case findWithRemainder f xs of
            (mf, ys) -> (mf , x : ys)

--- finds an element of a list and returns the element along
-- with other elemens adjacent to it
findWithAdjs :: (a -> Bool) -> [a] -> ([a] , Maybe a, [a])
findWithAdjs  f  []  = ([] , Nothing, [])
findWithAdjs  f (x:xs)
    | f x        =  ([] , Just x , xs)
    | otherwise  =
        case findWithAdjs f xs of
            (fs, mf, ys) -> (x: fs , mf , ys)



-- differentiateBy f xs ys: returns all elements in ys where
-- that are similar, after an application of f, to xs (in the first argument)
--- along with the remaining elemets of ys (in the second argument)
differentiateBy :: Eq a => (a -> a) -> [a] -> [a] -> ([a],[a])
differentiateBy f   []   ys    = ([],ys)
differentiateBy f    _   []    = ([],[])
differentiateBy f (x:xs) ys    =
    case findWithRemainder ((== x) . f) ys of
          (Nothing , zs)  ->  differentiateBy f xs zs
          (Just k, zs)    ->  (k : ps , qs)
            where  (ps, qs) =  differentiateBy f xs zs


---
--- findIndexesPresent f  xs  ys: finds the indexes of elements  xs
--- where f i is in ys
findIndexesPresent :: Eq a => (b -> a) -> [b] -> [a] -> [Int]
findIndexesPresent f   []   _     = []
findIndexesPresent f    _   []    = []
findIndexesPresent f (x:xs) ys    =
    case findIndex (== f x) ys of
          Nothing   ->  findIndexesPresent f xs ys
          Just k    ->  k : findIndexesPresent f xs ys
--}
--- repeatedly preforms differentiateBy over a list of elements
differentiateList :: Eq a =>  (a -> a) -> [[a]] -> [a] -> [[a]]
differentiateList  f    []    _ =  []
differentiateList  f (xs:xss) ys
    | null zs   = differentiateList f xss rest
    | otherwise = zs : differentiateList f xss rest
   where (zs, rest) = differentiateBy f xs ys

----------------------------------------------------------------------------------------------------
--  monotone decreasing lists
----------------------------------------------------------------------------------------------------
mtone3 :: Ord a => [a]  -> [a]
mtone3 (w:x:y:z:zs) | w >= x && x >= y && y >= z = y: mtone3 (z:zs)
                    | otherwise        = mtone3 (z:zs)
mtone3   _                                    = []
--
----------- minimum function that does not crash -------------------
myMin :: (Ord a) => [a] -> Maybe a
myMin  []   =  Nothing
myMin  xs   =  Just (minimum xs)

-- need to write this function
myMin' :: (Ord a) => [(a,b)] -> (a,b)
myMin'  []   =  error "minimum empty list"
myMin'  ((a,b):xs)  =  foldl  mF (a,b) xs
               where  mF (x,y) (p,q) = if x < p then (x,y) else (p,q)
----------------------------------------------------------------------------------------------------
-- | returns a list with the penultimate element removed along with the element
-- | that was removed. Only works for list of lenght greater than 2.
butLastElm :: [a] -> Maybe ([a],a)
butLastElm    [x,y,z] = Just ([x,z],y)
butLastElm    (x:xs)  = case butLastElm xs of
                         Nothing  -> Nothing
                         Just (ys,k) -> Just (x:ys, k)
butLastElm     _      =  Nothing

-- choose all sublists
subs                 :: [a] -> [[a]]
subs []               = [[]]
subs (x:xs)           = ys ++ map (x:) ys
                        where
                           ys = subs xs
---
subsN _ []               = [[]]
subsN n (x:xs)           = [ks | ks <- map (x:) ys ++ ys , length ks == n]
                           where
                              ys = subsN n xs

--- choose all possible sublists of xs with length less or equal to xs
choose xs  n  =  [ x | x <- subs xs , length x == n]

-- nSegs :: [a] -> > [[a]]
nSegs  _   []    =  []
nSegs  0    xs   =  [xs]
nSegs  n   xs@(_:ys)  | n >= length xs  =  [xs]
                      | otherwise       = take n xs : nSegs n ys


-- determine whether two lists have the same element
sameElements :: Eq a => [a]  ->   [a]   -> Bool
sameElements    []      []    =  True
sameElements    (x:xs)  ys    =  sameElements xs (delete x ys)
sameElements    _       _     =  False

----------------------------------------------------------------------------------------------------
--                            COMPUTING THE ADJUSTED MATRIX
----------------------------------------------------------------------------------------------------
-- ordered sub lists. The ordering is subject to the aprori arrangement of the list
oSubs :: [a] -> [[a]]
oSubs  []         =   []
oSubs  xs@(_:xs') =   [take i xs |  i <- [1..length xs]] ++ oSubs xs'

-- return all possible pieces, ignoring pieces  that treat individual points
-- as a line segment. I may have to add the possibility to treat at least the
-- final point as a piece when annalysing segments
allPieces :: Eq a => [a]  -> [[[a]]]
allPieces   xs  =  [ ys | let n = length xs,  i <- [1.. n], ys <- allOperms_aux i n xs]
   where allOperms_aux   _  _  []              = []
         allOperms_aux   n  m  xs  | n == 1    = [[xs]]
                                   | n == 2    = [[ys,zs] | i <- [1 .. length xs],
                                                    let (ys,zs) = splitAt i xs, not (null ys),
                                                    length zs > 1, length ys > 1] --   ]
                                   | otherwise = [(ns:ps) | i <- [1.. m],
                                                   let (ns, ms) = splitAt i xs,
                                                   let mm = length ms, length ns > 1,
                                                   ps <- allOperms_aux (n-1) mm ms]

-- | nPieces : all possible adjacent elements from n cuts of the list, excluding empty subslists
nPieces :: Eq a => Int -> [a] -> [[[a]]]
nPieces     n      xs
    | n == 0    =  [[xs]]
    | otherwise =  [(ns:ps) | i <- [1.. (length xs - 1)],
                              let (ns, ms) = splitAt i xs, not $ null ns,
                              ps <- nPieces (n-1) ms, all (not . null) ps]


---
-- | nPieces : all possible adjacent elements from n cuts of the list, excluding empty subslists
nPiecesF :: (Eq a, Ord b) => ([a] -> b) -> Int -> [a] -> [[[a]]]
nPiecesF  f   n      xs
    | n == 0    =  [[xs]]
    | otherwise =  [ (ns:ps) | i <- [2.. 5],
                              let (ns, ms) = splitAt i xs, not $ null ns,
                              ps <-  (nPiecesF f (n-1) ms)]
                              -- all (not . null) ps]

{-# INLINE nppP #-}
---- towards a more efficient  version of the npieces function
----  to be continued
nppP :: Int -> Int -> [a] -> [([[a]],[Int])]
nppP 0 lim xs  = [([xs],[])]
nppP 1 lim xs  = nppP' lim (length xs - lim + 1) xs --
nppP m lim xs  = [(ns : ps, i : ks) | i <- [lim .. (length xs - lim)]
                     , let (ns, ms) = splitAt i xs
                      -- now recursively build-up the tail with sublists longer than lim
                     , (ps,ks) <- nppP (m-1) lim  ms
                     , all ( < 10) (i : ks) ]
                   
{-# INLINE nppP' #-}
nppP' :: Int -> Int -> [a] -> [([[a]],[Int])]
nppP'  _  _  []   = []
nppP'  st en xs
    | st >= en   = []
    | null back  = [([front], [st])] -- ] : nppP' (st + 1) en xs
    | otherwise  = ([front,back], [st,length back]) : nppP' (st + 1) en xs
    where
        !ln      = length front
        (front,back) = splitAt st xs
----------------------------------------------------------------------------------------------------
--  | nPieces:  all possible adjacent adjacent elements from n cuts of the list built from
--  | least lim elements, keeping track of the occurrence of cuts
{-# INLINE nPiecesP #-}
nPiecesP ::  Int ->     -- ^ The number of splits for the lsit
             Int ->     -- ^ The minimum length of the shortest piece
             Maybe Int ->     -- ^ The maximum length of the longest piece (bar the first bit)
             [a] ->     -- ^ The list to split up
             [([[a]],[Int])]
nPiecesP    m    lim mLn xs
    | null xs   =  []
    | m == 0    =  [([xs],[])]
    -- | length xs <= m = [([xs],[])]
    | otherwise =  [(ns:ps, i : ks) |
                        i <- [lim .. (length xs - lim)]
                        -- get the first element, making sure it is longer than lim
                        , let (ns, ms) = splitAt i xs -- length ns >= lim,
                        -- now recursively build-up the tail with sublists longer than lim
                        , (ps,ks) <- nPP (m-1) lim mLn ms
                        , let pss = ns:ps
                        , maybe (all ((>= lim) . length) pss) (\n -> maxLn n pss) mLn
                   ]
    where
         nPP mn lm mlN ys = let npp = nPiecesP  mn lm mlN ys  in npp `pseq` npp
         maxLn mx = all (\xs -> let lm = length xs in (lm >= lim ) && (lm <= mx))
----------------------------------------------------------------------------------------------------
{-# INLINE nPiecesPF #-}
nPiecesPF :: (Ord a , Num a) => Int ->     -- ^ The number of splits for the lsit
             Int ->     -- ^ The minimum length of the shortest piece
             (a -> a) ->
             Maybe Int ->     -- ^ The maximum length of the longest piece (bar the first bit)
             [a] ->     -- ^ The list to split up
             [([[a]],[Int])]
nPiecesPF    m    lim f mLn xs -- =  maintainBy'  (sumBy (sumBy f) . fst) (Just 6) . nPiecesP    m    lim mLn
    | null xs   =  []
    | m == 0    =  [([xs],[])]
    -- | length xs <= m = [([xs],[])]
    | otherwise =  [(pss, i : ks) |
                        i <- [lim .. (length xs - lim)]
                        -- get the first element, making sure it is longer than lim
                        , let (ns, ms) = splitAt i xs -- length ns >= lim,
                        -- now recursively build-up the tail with sublists longer than lim
                        , (ps,ks) <- take 2 $  nPP (m-1) lim mLn ms
                        , let pss = (ns:ps)
                        , maybe (all ((>= lim) . length) pss) (\n -> maxLn n pss) mLn
                   ]
    where
         nPP mn lm mlN ys = let npp = nPiecesPF  mn lm f mlN ys  in npp `pseq` npp
         maxLn mx = all (\xs -> let lm = length xs in (lm >= lim ) && (lm <= mx))
----------------------------------------------------------------------------------------------------}

nPiecesPOpt :: Int ->     -- ^ The minimum length of the shortest piece
             -- Int ->     -- ^ The maximum length of the longest piece
             [a] ->     -- ^ The list to split up
             [([[a]],[Int])]
nPiecesPOpt   lim xs =
             [(ns:ps, i : ks) | i <- [lim .. (length xs - lim)],
                        -- get the first element, making sure it is longer than lim
                        let (ns, ms) = splitAt i xs , length ns >= lim,
                        -- now build-up the tail with sublists longer than lim
                        (ps,ks) <- splits lim lim (length xs - lim) ms, all ((>= lim) . length) ps]
    where
        splits :: Int -> Int -> Int -> [a] ->  [([[a]], [Int])]
        splits    _ _ _ []            =   []
        splits   mn m n xs
            | length xs <= n + m      = [ ([front,back],[m])]
            | m  >= n                 = splits mn mn n xs
            | otherwise               =
                case splits mn (m+1) n back  of
                    []  ->  [([front],[m]) ]
                    ((ps,zs) :rest) -> (front : ps, m : zs) : rest
                where
                    (front, back)     = splitAt m xs


--- maintain elements of a list that are valid by
maintainBy' :: (Eq a , Ord b ) => (a -> b) -> Maybe Int -> [a] -> [a]
maintainBy'   f   mb  = maybe (($!) (maintainBy f)) (\n -> myTake n . maintainBy f) mb

maintainBy :: (Eq a , Ord b ) => (a -> b) -> [a] -> [a]
maintainBy  _    []    = []
maintainBy  f   (x:xs) = maintainBy_aux f x [] xs
    where
        maintainBy_aux :: (Eq a , Ord b) => (a -> b) -> a -> [a] -> [a] -> [a]
        maintainBy_aux  _  _  acc   []    = acc
        maintainBy_aux  f  a  acc (y:ys)
            | f a <   f y = maintainBy_aux f a (a <:> acc) ys
            | otherwise   = maintainBy_aux f y (y <:> acc) ys
                where
                    (<:>) :: Eq c => c -> [c] -> [c]
                    (<:>)  x  []              =  [x]
                    (<:>)  x  ys@(y:_)
                                  | x == y    =  ys
                                  | otherwise =  x : ys

--- filter out "poor values" inroder to preserve a list of length n
maintainByLen :: (Eq a , Ord b) => Int  -> (a -> b) -> [a] -> [a]
maintainByLen  n f xs
    | n < length xs   =  maintainBy' f (Just n) xs -- xs
    | otherwise       = xs -- maintainBy' f (Just n) xs

----------------------------------------------------------------------------------------------------
-- return the first elements of a list when ther are all similiar by a given predicate
similarBy :: (a -> a -> Bool) -> [a] -> Maybe a
-- similarBy p  [a]      =  Just a
similarBy p  [a,b]
    | p a b           =  Just a
    | otherwise       =  Nothing
similarBy p  (a:b:c:xs)
    | p a b && p b c  =  Just a
    | otherwise       =  similarBy p (b:c:xs)
similarBy p    _      =  Nothing
----------------------------------------------------------------------------------------------------
-- nPieces2: Gets all possible combinations of adjacent n pieces, without single
-- pieces at the end. Suitable for MML2DS
nPieces2 :: Eq a => Int -> Int -> Int -> [a] -> [[[a]]]
nPieces2     n   m  len xs | n == 0    =  [[xs]]
                           | n == 1    =  [[ys,zs] | i <- [3 .. len],
                                           let (ys,zs) = splitAt i xs, -- not (null zs),
                                           length zs > 1, length zs <= m, length ys <= m]
                           | otherwise =  [(ns:ps) | i <- [3.. len],
                                           let (ns, ms) = splitAt i xs,
                                           length ms > 1, length ns <= m, -- length ms <= m,
                                           ps <- nPieces2 (n-1) m (len - i) ms]

--- filling zeroes to align the pieces -------------------------------------------------------------
fillZeroes_aux :: Num a => Int -> Int -> Int -> [[a]] -> [[a]]
fillZeroes_aux   _  _ _     []    =   []
fillZeroes_aux   n  m  i (xs:xss)
    | null xss && m == 0          =  [zeros (n - lxs) ++ xs]
    | i == 1                      =  inKtor : appZros : fillZeroes_aux n ml (i+1) xss
    | otherwise                   =  inKtor : zerosXz : fillZeroes_aux n ml (i+1) xss
    where
        lxs     = length xs
        ml      = m + lxs
        zeros k = [0 | _ <- [1..k]]
        nmxs    = n - ml
        appZros = (xs ++ zeros (n - lxs))
        zerosXz = (zeros m ++ xs ++ zeros nmxs)
        inKtor  = (zeros m ++ [1 | _ <- xs] ++ zeros nmxs)

fillZeroes :: Num a => [[a]] -> [[a]]
fillZeroes   xs  = fillZeroes_aux (length xs) 0 1 xs

-- Finally, adjusting the matrix. This is achieved by removing the
-- last indicator vector and adding it to the first indicator vector.
-- This operation is only performed for lists of length greater than 2.
adjustMatrix :: Num a => [[a]] -> [[a]]
adjustMatrix    xss  =  case butLastElm xss of
                         Nothing             ->  xss
                         Just ((ys:yss), zs) ->  (sub ys zs) : yss
                         where sub ps qs  =  zipWith (-) ps qs

---                           END OF ADJUSTED MATRIX DEFINITIONS
---------------------------------------------------------------------------------------------------
findCuts :: Ord a => [a] -> [[a]]
findCuts  xs
    | null xs        = []
    | null asenBlock = [xs]
    | otherwise      = front : findCuts rest
    where
        asenBlock     = [(ps,qs)  | (ps,qs) <- firstNs 1 xs, ascDes ps ]
        (front, rest) = last asenBlock
        --
        ascDes xs  = ascending xs || desending xs
        --
        firstNs :: Int -> [a] -> [([a],[a])]
        firstNs n xs
            | n > length xs  = []
            | otherwise      = (ys, zs) : firstNs (n+1) xs
            where
                (ys, zs) = splitAt n xs

---- remove increasing cuts
remIncrCuts :: Bool -> [[Double]] -> [[Double]]
remIncrCuts positive xs
    | not positive       =  xs
    | 1 >= length xs     = xs
    | otherwise          = (unZipp . map remIncrCuts_aux . zipp) xs
    where
        remIncrCuts_aux :: ([Double],[Double]) -> ([Double],[Double])
        remIncrCuts_aux  (xs,ys)
            | any  null [xs,ys] = (xs,ys)
            | last xs < head ys =
                case (find (/= 0) (reverse xs)) of
                    Nothing -> (xs, ys)
                    Just k  -> (xs , (mkAscending . map (minusUntil k)) ys)
            | otherwise         = (xs, ys)
        --- remove increasing cuts
        minusUntil :: Double -> Double -> Double
        minusUntil n m
           -- | n == 0    = minusUntil (n + 0.5) m
            | m <= n    = m
            | otherwise = minusUntil n (m - (n + 0.0001))
        -- make a set of points ascending
        mkAscending :: [Double] -> [Double]
        mkAscending (a : b : xs)
            | a <= b    = a : mkAscending ( b : xs)
            | otherwise =  b : (b + 0.12) : mkAscending (map (+ b) xs)
        mkAscending  xs  = xs



-- says if the elements of a list are in ascending
ascending_aux ((a,b,c,d):xs)
    | a <= b && b <= c && c <= d = ascending_aux xs
    | otherwise                  = False
ascending_aux     _              = True      --   monotone True

--ascending4 = ascending_aux . zipp4
--desending4 = not . ascending4 -- all (uncurry (>)) . zipp
ascending :: Ord a => [a ] -> Bool
ascending  = all (uncurry (<=)) . zipp --   monotone True
--
desending :: Ord a => [a ] -> Bool
desending  = all (uncurry (>)) . zipp
--desending'  = all (uncurry (>=)) . zipp

------------------------------------------------------------------------------------
--          FUNCTIONS FOR GRAPHING
------------------------------------------------------------------------------------
-- mkIntervals : takes a lsit of known predictions and a list of the
-- frequency of cuts and produces a lsits of predictions relating to
-- each cut, along with the length of the list
mkIntervals :: [a] -> [Int] -> [([a],Int)]
mkIntervals     []      _         = []
mkIntervals    predts     []      = [(predts, length predts)]
mkIntervals    predts    (n:ns)   = (front,length front): mkIntervals rest ns
               where (front,rest) = splitAt n predts

---------------
-- just like mkIntervals but considers a minimum number of points
mkIntervals1 :: [a] -> [Int] -> [[a]]
mkIntervals1     []      _         = []
mkIntervals1    predts     []      = [predts]
mkIntervals1    predts    (n:ns)   = front : mkIntervals1 rest ns
  --  | length rest <  4             =  front : [rest]
  --  | otherwise                    =
               where (front,rest)  =  splitAt n predts

-- calculates that function defined over the entire range
trendLine :: [Double] -> [Int] -> Double -> Double
trendLine ys = getFun . mkIntervals ys
           where
                getFun xs = getFun' xs 1
                f = fromIntegral
                getFun'  [(xs,_)]  k x                 = fun xs k x
                getFun'  ((xs,n):rest) k x
                            | x <= k - 0.9999999 + f n = fun xs k x
                            | otherwise                = getFun' rest (k + f n) x

predict :: [Double] -> [Int] -> Double -> Double
predict xs ns = predict_aux xs ns 1
          where
               predict_aux xs [] k = fun xs k
               predict_aux xs (n:ns) k = let (_,rest) = splitAt n xs in
                                             predict_aux rest ns (k + f n)
               f   = fromIntegral

-- computing a linear frunction defined over a monotone list of points
fun xs k  x = m * x +  c
          where
               m   = (last xs - head xs) / (fromIntegral (length xs) - 1)
               c   = head xs - m * k

---- list of length k with inverted predictions
getOppFuns :: Double -> [Double] -> [Vector Double]
getOppFuns k xs
    | null xs   =  []
    | otherwise =  zipWith mFl mfs (replicate (floor k)  [1 .. fLen xs])
    where
        mFl f   = fromList . map f
        hs      = head xs
        ms      = mean (fromList xs)
        fLen    = fromIntegral . length
        m       = (last xs - hs) / (fLen xs - 0.5)
        mf n x  =  (fst n) *  x  + (snd n - fst n)
        mfs     =  map mf $ zip (map  (m/) [-1 * k .. -1]) mkSlp
        mkSlp  | m >= 0       = [ms ,ms + (1/k) .. ]
               | otherwise    = [ms , ms - (1/k) .. ]

--- given a list of predictions, check that all of it is ascending.
--- if all are not, calculate the inversion of the descending slopes and
--- combine them up to form a larger list
manyInvNS :: Bool -> [Double] -> [[Double]]
manyInvNS positive  = take 1 . map toList. combWithInv positive . remIncrCuts positive . findCuts
    where

        combWithInv :: Bool -> [[Double]] -> [Vector Double]
        combWithInv  pos  []   =   []
        combWithInv  pos (x:xs)
            | pos && ascending x  =
                case combWithInv pos xs of
                    [] ->  [fromList x]
                    xss -> [vAdd (fromList x) k | k <-  xss]
            | (not pos) && desending x =
                 case combWithInv pos xs of
                    [] ->  [fromList x]
                    xss -> [vAdd (fromList x) k | k <-  xss]
            | otherwise    =
                case combWithInv pos xs of
                    [] ->   vss
                    yss ->  concatMap (\a -> map (flip vAdd a) vss)  yss
            where
                vAdd v1 v2  = join [v1,v2]
                vss         = getOppFuns 20 x

-----------------------------------
---returning the gradient of each new slope, which determins how steep the inversions are
getOppFuns1 :: Double -> [Double] -> [(Double ,Vector Double)]
getOppFuns1 k xs
    | null xs   =  []
    | otherwise =   [ (fst (yss !! i) , vs) | (yss,i)  <- zip mprs [0,1 .. length xs - 1]
                                              , let vs = fromList $ map snd yss
                                              , let vsum = foldVector (+) 0 vs
                                              , vsum /= 0 ]
    where
        mprs ::  [[(Double,Double) ]]
        mprs    = zipWith map mfs (replicate (floor k)  [1 .. fLen])
        ---
        hs1     = head xs
        hs      = if hs == 0 then 0.0000001 else hs1
        ms      = mean (fromList xs)
        fLen    = (fromIntegral . length) xs
        mm      = (last xs - hs) / (fLen - 0.5)
        mf (m,c) x  =  (m, m *  x + c)
        --
        mfs  :: [Double -> (Double , Double)]
        mfs     =  map mf $ zip (map  (mm/) [-(2 * k) .. -1]) intercept
        intercept  | mm <= 0      =  iterate ((+) (1/k))  ms
                   | otherwise    =  iterate ((-) (1/k))  ms

-- given a list of predictions, returns a lists where the portions of the predictions
-- with negative slopes are inverted (in the sense that the least possible positive slope is
--  drawn where slope is not zero)
manyInvNS1 ::  Bool ->  [Double] ->  [(Double ,[Double])]
manyInvNS1 pos = map (\(a,b) -> (a,toList b)) . combWithInv pos . remIncrCuts pos . findCuts
    where
        --
        combWithInv :: Bool -> [[Double]] -> [(Double , Vector Double)]
        combWithInv  pos []       =   []
        combWithInv  pos (x:xs)
            | (pos && ascending x) || ((not pos) && desending x)  =
                case combWithInv pos xs of
                    [] ->  [ (1, fromList x)]
                    xss -> [ (i , vAdd (fromList x) k) | (i,k) <-  xss]
            | otherwise    =
                case combWithInv pos xs of
                    [] ->   vss
                    yss ->  concatMap (\(j,a) -> [(j,flip vAdd a k) | (_,k) <- vss])  yss
            where
                vAdd v1 v2  = join [v1,v2]
                len         =  length x
                ln          = if len < 20 then 15 else 20 
                vss_aux     = getOppFuns1 (fromIntegral ln) x
                vss         = maybe [(0,fromList x)] (:[]) (listToMaybe vss_aux)


-----------------------------------------------------------------------------------------------
{--
   allPositiveSlopes : takes a list of known predictions, paired with
   a list frequency of cuts in the predictions, and decides whether all
   slopes among the predictions are >= 0. The function returns
   True if all slopes are at least 0 and False otherwise.
--}
allPositiveSlopes :: [Double] ->  [Int] -> Bool
allPositiveSlopes  predts cuts  =  all id slopes 
    where
        slopes                  = [ascending points | (points, _) <- mkIntervals predts cuts]
        gradient xs | null xs   = 0
                    | otherwise =  m xs
        m  xs = (last xs - head xs) / (fromIntegral (length xs) - 1)



---------------------------------------------------------------------------------------------------
--- takes a list of functions, a list of split positions and a
--- list of the input (i.e x) values for the functions
piecewiseFun :: [(Double -> Double)] -> [Int] -> [Double] -> Double -> Double
piecewiseFun  xs@(f:_)  []   _  k             =  f k
piecewiseFun  (f:fs) (n:ns) ys  k | k < x     =  f k
                                  | otherwise =  piecewiseFun fs ns rest k
    where
        x =  (ys !! n-1)
        (_,!rest) = splitAt n ys


--- predictions from linear parameters
predsFromLP :: [Double] -> [Int] -> Double -> Double
predsFromLP   [c,m]  []  x      = c + m* x
predsFromLP   (c:m:xs) (y:ys) x
    | x <= (fromIntegral y)     = c + m * x
    | otherwise                 = predsFromLP xs ys x
predsFromLP     _    _   x      = x

-- factorize then common----}
--
appFunAfterFactoring :: Eq a => (a -> b) -> [[a]] -> [[b]]
appFunAfterFactoring   f  xs = concat [appF f zs | zs <- factorHead xs]
    where
        appF :: (a -> b) -> Either (a,[[a]]) [a] -> [[b]]
        appF f eth = case eth of
                         Left !ps  -> multiplyThrough f ps
                         Right !qs -> [map f qs] 
        --
        multiplyThrough :: (a -> b) -> (a, [[a]]) -> [[b]]
        multiplyThrough  f  (x, xs)  =  fx `seq`  map ((fx:) . map f) xs
                         where   fx  =  f x
        --
factorHead :: Eq a => [[a]] -> [Either (a,[[a]]) [a]]
factorHead  []                       =  []
factorHead  ((x:xs):ys) | null  ys   =  [(Right (x:xs))]
                        | null front =  (Right (x:xs)) : factorHead back
                        | otherwise  =  (Left (x,xs: map tail front)) : factorHead back
   where (front,back) = span ((== x) . head) ys

--------------------------------------------------------------------------------------------------
findGrps :: ( Eq a) => Double -> Int -> (a -> a ->  Double)  ->  [a] -> [[Int]]
findGrps tol lim f xs   -- move sg and replace it with an integer reppresenting the maximum ot group
    | tol <= 0.2           = groups: [ js | let lss = preCombine1 False groups, js <- [groups,lss]]
    | length groups < lim  = [ ks | ks <- recomnFix groups]
    | otherwise            =  findGrps (tol - 0.05) lim f xs
    where
        groups  =  findGrps_aux  1 f tol xs
        -- possCuts -- groups elements together to possiblt form a group
        possCuts m n xs
            | n > (length xs - m)  =  []
            | otherwise =  (front, length front) : possCuts m (n+1) xs
            where front = take n xs
        --- add
        preCombine1 :: Bool ->  [Int] -> [Int]
        preCombine1 bb (x:y:z:xs)
           -- | null xs            =
            | all1 [y,z]   =  preCombine1 bb (x : 2 : xs)
            | x == 1       =  preCombine1 bb ((1 + y) : z : xs)
            | y  == 1       = preCombine1 bb ((x + 1) : z : xs)
            | all1 [y,z] && x == 1 = preCombine1 bb (3 : xs)
            | all1 [x,y]   =  preCombine1 bb (2 : z : xs)
            | otherwise          = x : preCombine1 bb (y : z : xs)
            where
              all1 :: [Int]  -> Bool
              all1  xs  = all (== 1) xs
        preCombine1   _  xs    =  xs
        ---
        findGrps_aux :: ( Eq a) => Int -> (a ->  a ->  Double) -> Double -> [a] -> [Int]
        findGrps_aux _ _  _      []      = []
        findGrps_aux _ _  _     [a]      = [1]
        findGrps_aux  mn f  trh ks@(x:xs)  = mxx : findGrps_aux mn f trh (drop mxx ks)
            where
                mxx      = maybe mn (\_ -> maximum mkGrps) (listToMaybe mkGrps)
                mkGrps   =  [ n | (ys,n) <- possCuts mn mn ks
                                     , let rss = map (f x) ys
                                     , all (>= trh) rss
                                     ]
        --- recombinining with fixed groups
        recomnFix :: [Int] -> [[Int]]
        recomnFix cls
            | lcls <= 25 = combinations 0 2 cls
            | lcls <= 32 = combinations 7 (lim `div` 15) cls
            | otherwise  = combinations 8 (lim `div` 10) cls
            where
                lcls     = length cls
                combinations m k xs = [kss | i <- [m .. 19], (ks,_) <- nPiecesP i k Nothing xs , kss <- [map sum ks]]

----------------------------------------------------------------
-- returns the first elements that satisfies a given predicate
takeFirst :: (a -> Bool) -> [a] -> [a]
takeFirst    f   =  myTake 1 . filter f

-- alternative takeFirst returning maybe a
takeFirstM :: (a -> Bool) -> [a] -> Maybe a
takeFirstM    f    = listToMaybe . takeFirst f
-----------------------------------------------------------------------
-- change the suffix of an inout file
in2out :: String -> String
in2out  ".dat"    =  ".nrm"
in2out   (x:xs)    =  x : in2out xs
in2out     xs      =   xs


-- tokens
tokens :: String -> Char -> [String]
tokens   []     a   = []
tokens  (x:xs)  a   | x == a     =  tokens xs a
                    | otherwise  =  let (front, back) = span ( /= a) xs  in
                                        (x:front) : tokens back a

-- make tokens of a list of a given number of elements
tokeNn :: Int -> [a] -> [[a]]
tokeNn   _   []    =  []
tokeNn   n   xs    =  y : tokeNn n ys
    where (y , ys) = splitAt n xs

-- puts a character between every element of a list of strings
rToks :: [String] -> Char -> String
rToks   (x:xs)     a     =  x ++ foldr (\bs as -> (flip (:) bs a) ++ as) "" xs
rToks     []       _     =  []

--- rDoubleS: takes a lists of numbers as strings and returns then as list of Double
rDoubleS ::  [String] -> [Double]
rDoubleS                 =  map rDouble


rDouble :: String -> Double
rDouble                 = read

-- does the opposite to rDoubles
showDoubles :: [Double] -> [String]
showDoubles             =  map show



tokens' = flip tokens
-- i want the files in the .nrm file to be standardised

------------------------------------------------------------------------------------------------
printElmChr :: Show a => Char -> [a] -> String
printElmChr chr = foldl'  prt  []
    where prt b a | null b    = show a
                  | otherwise = b ++ (chr : show a)

printElm :: Show a => [a] -> String
printElm = printElmChr ','
---------------------------------------------------------------------
printStrings :: [String] -> Handle ->  IO ()
printStrings     []     _       =  return ()
printStrings   (x:xs)   handle  =  do hPutStrLn handle x
                                      printStrings xs handle
-- removes the brackets from a string
remSqBrcs       xs   =  filter (\a -> a /= ']' && a /= '[') xs
