{-# LANGUAGE BangPatterns #-}
module LINreg2 (mmlLinearRegLP, diagl, rowM, colM,(<+>), (<->), foldl'Rnf
                , mkZ,realignedNpieces,lineFun, mmlCorr-- ,vmean,vvar,vstDiv,vvarM,vunStdNorm,vstNorm, corrRoundCentre
                , standardizedFit,relFit, corr,nCorrV,logg
                ) where

import Numeric.LinearAlgebra
import Numeric.GSL.Statistics as S
import Control.DeepSeq (NFData, rnf)
import Numeric.GSL.Statistics (mean,variance,variance_m,stddev)
import Control.Parallel (pseq)
import Data.Ord (comparing)
import Data.List (transpose, nub,minimumBy,foldl')
import LMisc 
import Data.Time
import IO

ident n = (diag . fromList) (take n $ repeat 1) :: Matrix Double

vector xs = fromList xs :: Vector Double
diagl = diag . vector
rowM = asRow . vector
colM = asColumn . vector
-------------------------------------------------------
foldl'Rnf :: (NFData a) => (a -> b -> a) -> a -> [b] -> a
foldl'Rnf f z xs = lgo z xs
    where
        lgo z   []      = z
        lgo z (x:xs)    = lgo z' xs
            where
                z' = rnf (f z x) `pseq` (f z x)


------ standardise by subtracting the mean and dividing by the standard deviation
vstNorm       xs            = mapVector (\ a -> (a - m) / std) xs
                 where m    = mean xs
                       std  = sqrt $ variance_m m xs

-- unstandardising
vunStdNorm m std vs  = mapVector (\a -> std * a + m)  vs

--mean,variance,variance_m,stddev)
vunStdNorm' vs = vunStdNorm (mean vs) (stddev vs)  vs
-----------------------------------------------------------------------------------------------
logg a      --     = log a
      | a <= 0     = a
      | otherwise  = log a
-------------------------------------------------------
infix 1  <+>
(<+>) :: Matrix Double -> Matrix Double -> Matrix Double
m1  <+>    m2   = fromRows $ zipWith (zipVectorWith (+)) (toRows m1) (toRows m2)

(<->) :: Matrix Double -> Matrix Double -> Matrix Double
m1  <->    m2  =  fromRows $ zipWith (zipVectorWith (-)) (toRows m1) (toRows m2)
--}

-- adds a to the diadonal elements of ma
mkZ :: Double -> Matrix Double -> Matrix Double
mkZ  a   ma  = mapMatrixWithIndex (pli a) (trans ma <>  ma)
    where  pli b (!i,!j) v = if i == j then v + b else v


mmlLinearRegLP :: Vector Double ->  Double -> Double -> Matrix Double -> (Double, (Vector Double, (Double, Vector Double))) -- Vector Double
mmlLinearRegLP     xs     m   r  ys  =  (ans, (xTy, (sig, abar)))
                where
                  z       =   mkZ m ys -- make the Z matrix
                  n       =   fromIntegral $ rows  ys
                  !mM     =   n + k  + 1
                  !rest   =   log r + 0.5 * log (2 * mM * abs (det z)) + (n / 2) -- cd (k+1)
                  --abar   =   (inv z) <> (trans ys) <>  xs --
                  abar_aux    =    toColumns $ linearSolveLS z (asColumn ((trans ys) <>  xs))
                  abar    =  head abar_aux
                  !k      =   fromIntegral $ cols z
                  sigSq   =    (xs <.> xs)  - ( xs <.> (ys <> abar))
                  !sig    =   sqrt (sigSq / n)
                  xTy     =     (ys <>  abar) -- predictions fromList . manyInvNS1 Nothing $ toList
                  ans     =   (mM/2) * log (2 * pi) + n * log sig - 0.5 * (k + 1) * log k + rest -- k is 2 here
----
mmlMLwithAbar ::Vector Double ->  Vector Double ->  Double -> Double -> Matrix Double -> Double
mmlMLwithAbar abar     xs     m   r  ys  =  ans
                where
                  z       =   mkZ m ys -- make the Z matrix
                  n       =   fromIntegral $ rows  ys
                  !mM     =   n + k  + 1
                  !k      =   fromIntegral $ cols z
                  !rest   =   log r + 0.5 * logg (2 * mM * abs (det z)) + (n / 2) -- cd (k+1)
                  sigSq   =   (xs <.> xs)  - ( xs <.> (ys <> abar))
                  !sig    =   sqrt (sigSq / n)
                  xTy     =     (ys <>  abar) -- predictions fromList . manyInvNS1 Nothing $ toList
                  ans     =   (mM/2) * log (2 * pi) + n * log sig - 0.5 * (k + 1) * log m + rest


--- realigning a matrix
--- map mkMatrix : [[(Matrix Double,[Int])]]
realignedNpieces :: Int ->  --- number of interventions
    Int ->                  --- minimum length of each sub group
   -- Int ->                  -- maximum length of each subgroup
    [[Double]] ->             --- the list of values
    [(Matrix Double,[Int])] --- the matrix for analysis, along with the locations of interventions
realignedNpieces n m  =  nub . concatMap combMatrix . transpose . map  mkMatrix -- map (\ks -> (newMX, ks)) ctts
    where
        ---
        combMatrix :: [(Matrix Double,[Int])] -> [(Matrix Double, [Int]) ]
        combMatrix mcI  = map (\ks -> (newMX, ks)) cts
            where
                (mxx, cts) = unzip mcI
                newMX      = appendByColumns mxx
        mkMatrix :: [Double] ->  [(Matrix Double,[Int])]
        mkMatrix xs      =   [(reAlign ps ,ks) |
                                let nPnms  =  nPiecesP n m Nothing xs
                                , (ps,ks) <-   nPnms 
                                ]  
            where
                nPns             = [] 
                len              = length xs
                cons a vs        = join [fromList [a] , vs]
                app2rows f       = fromRows . map f  . toRows
                tS'              = app2rows (cons 1) . trans . app2rows vstNorm . fromLists --vstNorm
                reAlign          = tS' . adjustMatrix . fillZeroes_aux len 0 1
appendByColumns :: Element t => [Matrix t] -> Matrix t
appendByColumns  = fromRows . concatMap toRows

-- draw a best fit line through a list of points
lineFun :: Vector Double -> (Double,Double)
lineFun vs = (ks @> 1, ks @> 0)
    where
        mx          =  (fst . head . realignedNpieces 0 1 . (: [])) [1 .. fromIntegral $ dim  vs]
        (_,(_,ks))  =  snd $ mmlLinearRegLP (vstNorm vs) 1 1  mx

--
-- finding mormalized correlation using  using MML
mmlCorr :: [Double] -> [Double] -> Double
mmlCorr xs ys =  nCorrV (fromList xs) (fromList ys)


mmlCorr1  xs ys =  (mmlCorr_aux (Just n) xs ys)/ (mmlCorr_aux Nothing [1 .. fromIntegral n] [1 .. fromIntegral n])
    where
        n           = (minimum . map length) [xs, ys]
        mmlCorr_aux mN xs' ys' = (ks @> 1)
            where
                stNorm      =  toList . vstNorm . fromList
                yss         =  (stNorm . maybe ys' (flip take ys')) mN
                vss         =  (vstNorm . fromList . maybe ys' (flip take xs')) mN
                (_,(_,ks))  =  snd $ mmlLinearRegLP vss 1 1  mx
                mx          =  (fst . head . realignedNpieces 0 1 . (: [])) yss

-- relative fit of twoe straight line functions
relFit :: [Double] -> [Double] -> Double
relFit xs ys   = relFit_1 (vstNorm $ fromList xs) (vstNorm $ fromList ys)
relFit_1  xs ys =   abs ((mly  - mlx)  /  mlx)
    where
        mly    = mmlMLwithAbar ab  ys  1 1  (mx ys)
        (mlx,(_,(_,ab))) =   mmlFun  xs
        mx   xs'     =  (fst . head . realignedNpieces 0 1 . (: [])) [1 .. fromIntegral $ dim xs']
        mmlFun vs    =  mmlLinearRegLP vs 1 1  (mx vs)

---pearsons correlation
corr xs  ys  =  S.correlation (fromList xs) (fromList ys)

-- normalised correlation
nCorrV xs ys  = (S.correlation (subVector 0 n xs) (subVector 0 n ys))
    where
         n    =  min (dim xs) (dim ys)


---
standardizedFit  :: [Vector Double] -> Double
standardizedFit [] = 0
standardizedFit ks@(_:_) = S.stddev . fromList $ map mSgLn ks
    where
        mSgLn  ys    = mmlMLwithAbar ab  ys  1 1  (mx ys)
        (_,(_,(_,ab)))   = minimumBy (comparing fst) $ map mmlFun ks -- (fst . snd . snd)
        mx   xs'     =  (fst . head . realignedNpieces 0 1 . (: [])) [1 .. fromIntegral $ dim xs']
        mmlFun vs    =  mmlLinearRegLP vs 1 1  (mx vs)

