{-# LANGUAGE BangPatterns #-}
module ListStats (mean, var, stDiv, nCr,  varM, qf,unStdNorm,unStdNormSplits, -- cov, corr,
                  stDivM, stNorm, erf, cn, erfInv, binomial, cdf, cdF, meanBy, stDivBy,
                  normal, gammaln,  adjustedMatrix, adjustedMatrixN, adjustedMatrixPN, normalP,
                  module ListAlgebra,
                 ) where

import Numeric
import Numeric.LinearAlgebra
import qualified Data.Vector as V (map, fromList)
import Data.List 
import Control.DeepSeq (NFData)
import LINreg2 (foldl'Rnf )
import Data.Ord (comparing)
import ListAlgebra

----------------------------------------------------------------------------------------
--- mean, variance and correlations
----------------------------------------------------------------------------------------

insertZeros :: Num a =>  [a] -> [Int] -> [a]
insertZeros xs   []   = xs
insertZeros xs (a:as) =
        let   (front,back) = splitAt (a-1) xs  in
            (1 :front) ++ (0 : insertZeros back  as)

-- map a function then calculate the standard deviation
stDivBy :: (a -> Double) -> [a] -> Double
stDivBy f xs = (sumBy (sqr . (\a -> a - ns) . f) xs) / k
    where
       (sm,k) = foldl' (\(n,m) a -> (f a + n, m + 1)) (0,0) xs
       ns     = sm / k
       sqr x  = x * x

-- map a function then calculate the mean
meanBy :: (a -> Double) -> [a] -> Double
meanBy g xs = sm / k
   where (sm,k) = foldl' (\(n,m) a -> (g a + n, m + 1)) (0,0) xs

----
mean  :: (NFData a, Fractional a) => [a] -> a
mean     xs                    = fst suml / (snd suml - 1)
                    where suml = foldl'Rnf  kF (0, 1) xs
                          kF (!n , !r) b =  (n + b, 1 + r)					
---
var  :: (NFData a, Floating a) => [a] -> a
var     xs                    =  (fst suml / snd suml)
                   where suml = foldl'Rnf  kF (0, 0) xs
                         kF (!n , !r) b = (n + (b - k)^2 , 1 + r)				
                         k    = mean xs
-- variance of a list of values with the mean given
varM  :: (NFData a , Floating a) => a -> [a] -> a
varM     k  xs              =  fst suml / snd suml
                 where suml = foldl'Rnf  kF (0, 0) xs
                       kF (!n , !r) b = (n + (b - k)^2 , 1 + r)				
--
--- the standard deviation without the Mean given
stDiv   :: (NFData a , Floating a) =>  [a] -> a
stDiv                       = sqrt . var

---- standard deviation with the mean given
stDivM        k             = sqrt . (varM k)


--- standardise by subtracting the mean and dividing by the standard deviation
--stNorm :: [Double] -> [Double]
stNorm       xs             = map (\ a -> (a - m) / std) xs
                 where m    = mean xs
                       std  = stDivM m xs
---
unStdNorm  xs ys  = map (\a -> std * a + m)  ys
                where  m    = mean xs
                       std  = stDivM m xs

unStdNormSplits  xs zs = unStdNorm (insertZeros xs zs)

---   selecting the 'best' sublist of lenght n of a list of values
bestN :: (Ord a , Ord b)  => (a -> b) -> Int -> [a] -> [a]
bestN  comp   lim  xs
    | length xs <= lim   =  xs
    | length back <= lim =  myBest lim comp xs
    | otherwise          =  min (sortBy (comparing comp) front) (bestN comp lim back)
    where
        (front, back) = splitAt lim xs
        myBest n  f   = myTake n .  sortBy (comparing f)
        mn            = lim `div` 2

--- computed the weighted average of the residuals of values, which represent
--- points on the y-axis plotted against x-values [1 .. n], where n is the
-- length of the list. The weight biased towards is determind as the
wgtAvgRd ::  (NFData a, Enum a, Fractional a) => Int -> [[a]]  -> a
wgtAvgRd    n   xs     = weightsSum / len
    where
        (weightsSum , len) = foldl'Rnf (getWeightedAvg (2 * n)) (0,0) xs
        getWeightedAvg k (wa,ln) ys  =   (wa + wVal , ln  + 1)
            where
                f     = fromIntegral
                len   = f $ length ys
                ks    = [1 .. len]
                sumSqrs  as bs = foldl'Rnf (\a (b,c) -> a + b * c) 0 (zip as bs)
                wVal  =  (sumSqrs ks ys) / (sumSqrs ks ks)   -- (f k / len)  *

--------------------------------------------------------------------------------------------
--  Defining the X matrix for the MML function
--------------------------------------------------------------------------------------------
-- We combine adjustMatrix, fillZeros and allPieces to
-- give the final adjusted matrix. This matrix is standardised, then transposed,
-- but the transposeition can be removed if we require that to occur later on
adjustedMatrix  xs   =  [( tS . adjustMatrix . fillZeroes_aux n 0 1) yss | yss <- allPieces xs]
                         where n  = length xs
                               tS =  fromLists . map (1:) . transpose . map stNorm

adjustedMatrixN m xs =  [tS $ adjustMatrix $ fillZeroes_aux n 0 1 yss | yss <- nPieces m xs]
                         where n  = length xs
                               tS = map (1:). transpose . map stNorm
adjustedMatrixPN k m xs = [(tS $ adjustMatrix $ fillZeroes_aux n 0 1 yss, cuts) |
                                (yss,cuts) <- nPiecesP k m Nothing xs, not $ null cuts]
                         where n  = length xs
                               tS = map (1:). transpose . map stNorm
--
-- test function
joins :: Eq a => [a] -> [[a]]
joins  xs | null xs = [] -- && (length (concat a) > 5)
          | otherwise = joins_aux xs ++ joins (tail xs)
    where
        joins_aux (x : y : ys) = ([x,y] :  (map (x :) $ joins_aux (y : ys)))
        joins_aux  _           = []
--------------------------------------------------------------------------------------------
-- calculating the binomial coefficient, and normal prob density
-------------------------------------------------------------------------------------------
nCr ::  (Integral a, Ord a) => a -> a -> a
nCr   n    r   | n < 0 || r < 0  =  error "can find combinations for negative numbers"
nCr   n   0                      =  1
nCr   0   _                      =  0
nCr   !n   !k                    =  nCr (n-1) (k-1) * n `div` k


--- binomial distribution
binomial :: (Integral a, Ord b, Fractional b, Ord a) => a -> a -> b -> b
binomial   n   r   p  | p > 1 || p < 0   = error "Invalid probability value" ---n nCr r *
binomial   n   k   p  | k >= 0 && k <= n = let  nck = fromIntegral (nCr n k) in nck * (p^k) * (1 - p)^(n - k)
                      | otherwise        = 0

-- value of x from a normal distribution with mean 'mn' and standard deviation "std"
normal x mn std = (1 / (std * sqrt (2 * pi))) * exp(- 0.5 * ((x - mn)^2) / (std ^2))
--normal x mn std = cdf (x + eps) mn std  - cdf (x - eps) mn std
    where
        eps = 0.0101095
        --z   = (x - mn) / std

-- calculating the normal with a given precision
normalP eps x mn std = cdf (x + eps) mn std  - cdf (x - eps) mn std
------------------------------------------------------------------------------------------------
--- approximating the error functions and its inverse
----------------------------------------------------------------------------------------------							
erf	x	| x  <  0    =  -1 *  erf (- 1 * x)
		| otherwise  = 1 - t * exp k
    where
        t  = 1 / (1 + 0.5 * abs x)
	t3 = t * (- 0.82215223 +  t *  0.17087277)
        t2 = t * ( 0.27886807 +  t * (-1.13520398 +  t *  1.48851587 + t3))
        t4 = t * ( 0.37409196 +  t * ( 0.09678418 +  t *  (- 0.18628806) + t2))
	k  = - x * x -  1.26551223 + t *  1.00002368 +  t4
--

--cn  :: Int -> Double		
cn	 0	=	1
cn	 n	=	foldr (\k  b -> b + (cn k  * cn (n - 1 - k)) / ((k + 1) * (2 * k + 1))) 0 [0.. n - 1]

-- inverse of the error function
--erfInv :: (Fractional a, Floating a, Enum a) => a -> a
erfInv	y | y <  -1 || y > 1  = error "erfInv argument out of range"
          | abs y == 1        = pol (-y* log (0.0))
          | y < - 0.7         = pol (- 1 * folD z (head c) (tail c) / folD z 1 d)
          | y <   0.7         = pol ( y *  folD (y^2) (head a) (tail a) / folD (y^2) 1 b)
          | otherwise         = pol (folD z1 (head c) (tail c)  / folD z1 1 d)
                  where a     = [0.886226899, -1.645349621,  0.914624893, -0.140543331]
                        b     = [-2.118377725,  1.442710462, -0.329097515,  0.012229801]
                        c     = [-1.970840454, -1.624906493,  3.429567803,  1.641345311]
                        d     = [3.543889200,  1.637067800]
                        z     = sqrt(- log ((1.0+y)/2.0))
                        z1    = sqrt(- log ((1.0+y)/2.0))
                        pol x = x - (erf(x) - y) / (2.0 /(sqrt pi)  * exp(-x*x))
                        folD  zz a ls = foldl'Rnf  (\a b -> a + zz * (fst b + snd b * zz)) a (zipp ls)


-- commulative density function
cdf   x mean std | std < 0      = error"Negative variance to CDF"
cdf   x mean std | otherwise    =  0.5 * (1 + erf ((x - mean) / (std * sqrt 2)))


-- the CDF for the standard normal distribution
cdF  n  =  cdf  n 0 1

-- the q function, which is 1 - CDF (x) (here given for the standard normal dist)
qf  x  =  1 - cdF x
--------------------------------------------------------------------------------------
--- The gamm function takel  from wikipedia
--------------------------------------------------------------------------------------
gammaln :: Double -> Double
gammaln xx = -tmp' + log(2.5066282746310005 * ser' / xx)
			where
			tmp' = (xx+5.5) - (xx+0.5)*log(xx+5.5)
			ser' = ser + sum (zipWith (/) cof [xx+1..])
			cof  = [76.18009172947146,-86.50532032941677,24.01409824083091,-1.231739572450155,0.001208650973866179,-0.000005395239384953]
			ser  = 1.000000000190015
