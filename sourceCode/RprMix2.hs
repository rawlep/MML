{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE UnboxedTuples #-}
{-----------------------------------------------------------------------------------------

          Computing the Minimum Message Length function
                   Rawle Prince, February 2012


             calculating the mixture models from the MML2ds functions
------------------------------------------------------------------------------------------}
module RprMix2 (alph, sigD, sigE, tau, r_n, aBar,sim_eq,
               mmlMixture, initModel,  initModelR , --- initModelGD,
               oMod, oModMl, negGradients, negGradientsM, negGradientsOM,
               updateModel, tDupdate,  validOM, valid,nextModelR',dataError,
               vmean,vvar,vstDiv,vvarM,vunStdNorm,printElm,vstNorm,invS,invNeg,
               -------- datatpes and modules ----
               Predicted, Data,  RNs, OModel (Omod),Mmodel (MkModel),LPars,
               module System.Random
              ) where
--
import Control.Concurrent (forkIO)
import ListStats (normal, mtone3,cdf)
import LINreg2 (foldl'Rnf) 
import LMisc (myTake,printElm,printElmChr,similarBy,findCuts, ascending,manyInvNS,sumBy) 
--import Prelude
import System.IO.Unsafe
import Numeric.LinearAlgebra
import Numeric.GSL.Statistics (mean,variance,variance_m,stddev)
import Control.Parallel (pseq)
import Numeric
import System.Random
import Foreign.Storable
import Control.DeepSeq
import Data.Time
import Data.List
import Data.Maybe (isJust, fromJust, listToMaybe)
import Data.Ord (comparing)
import qualified Data.ByteString.Lazy.Char8 as L
import System.Directory (doesFileExist)
import IO
-----------------------------------------------------------------------------------------------
instance (Storable a, NFData a) => NFData (Vector a) where
    rnf va = rnf (toList va)

instance NFData (Mmodel) where
    rnf (MkModel a b c vs d) = rnf (a, b, c ,vs, d)

instance NFData (OModel) where
    rnf (Omod m a) = rnf (m , a)

-----------------------------------------------------------------------------------------------
--- some statistics using vectors instead of lists
----------------------------------------------------------------------------------------
--- mean, variance and correlations
----------------------------------------------------------------------------------------
vmean  ::  Vector Double -> Double
vmean  = mean
---
vvar  :: Vector Double -> Double
vvar    = variance

-- variance of a list of values with the mean given
vvarM  :: Double -> Vector Double -> Double
vvarM    = variance_m

--- the standard deviation without the Mean given
vstDiv   = stddev

------ standardise by subtracting the mean and dividing by the standard deviation
vstNorm       xs            = mapVector (\ a -> (a - m) / std) xs
                 where m    = vmean xs
                       std  = sqrt $ vvarM m xs

-- unstandardising
vunStdNorm m std vs  = mapVector (\a -> std * a + m)  vs

-- building the mixture model
-----------------------------------------------------------------------------------------------

--- the model: alphs, sigE, sigD,  linear parms
data Mmodel = MkModel !Double !Double !Double (Vector Double) !Double deriving (Eq)
-- the data and the error class cannot have the same mean, so we need to learn that as well


-- if two models are close to each other
sim_eq  (MkModel a b c _ z) (MkModel p q r _ z1) =
        near a p && near b q &&  near c r  && near z z1 -- && etc
                  where near x y  =  abs (x - y) <= 0.008
      

--- the data values
type Weight      = Vector Double
type Predicted   = Vector Double
type Data        = Vector Double
type RNs         = Vector Double -- [Double]
type LPars       = Vector Double
type Rn          =  Double
type Cost        =  Double
type Lambda      =  Double
type Alpha       =  Double

-- projecting the compponents of the model
alph, sigD, sigE, tau :: Mmodel -> Double
alph   (MkModel a _ _ _ _)   =  a
sigD   (MkModel _ s _ _ _)   =  s
sigE   (MkModel _ _ e _ _)   =  e
tau    (MkModel _ _ _ _ z)   =  z

aBar :: Mmodel -> Vector Double
aBar   (MkModel _ _ _ a _)   =  a


negGradients :: Vector Double -> Bool
negGradients xs = True -- all negGD (zip xs [1..])


negGradientsM m = negGradients (aBar m)
negGradientsOM om = negGradientsM (oMod om)


-- update a model by a fraction of another - gradient decent update
updateModel :: Mmodel -> Double -> Mmodel -> Mmodel
updateModel  mO   lambda   mN  = MkModel nalph nsigD' nsigE' naBar ntau
                 where !nalph   = alph mO + lambda * (alph mN)
                       !nsigD   = sigD mO + lambda * (sigD mN)
                       !nsigE   = sigE mO + lambda * (sigE mN)
                       !ntau    = tau mO  + lambda * (tau mN)
                       !naBar   = zipVectorWith (+)  (aBar mO) (mapVector (lambda *)  (aBar mN))
                       --dehC     =  alpha * (c1 - c0)
                       nsigD'   = min nsigD nsigE
                       nsigE'   = max nsigD nsigE

--   update--- need to fix this to include lambda
tDupdate:: Mmodel -> Alpha -> Lambda -> Cost -> Cost ->  Mmodel -> Mmodel
tDupdate  mO alpha lam c0 c1    mN  = MkModel nalph nsigD nsigE naBar ntau
                 where nalph   =  alph mO + dehC * (alph mN - alph mO)
                       nsigD   =  sigD mO + dehC * (sigD mN  -  sigD mO )
                       nsigE   =  sigE mO + dehC * (sigE mN  -  sigE mO)
                       -- !nsigE   =  lam * sigE mO + dehC * (sigE mN - sigE mO)
                       ntau    =   tau mO  + dehC * (tau mN  -  tau mO)
                       naBar   = zipVectorWith (+) abar (mapVector (dehC *) (zipVectorWith (-) (aBar mN) abar))
                       dehC     =  alpha * (c1 - c0)
                       nsigD'   = min nsigD nsigE
                       nsigE'   = max nsigD nsigE
                       abar     = aBar mO
----

valid:: Mmodel -> Bool
valid (MkModel a b c _ e) = valAlph && valSigs && all ((/= "NaN") . show) [a,b,c]
     where
           !valAlph = a >= 0 && a <= 1
           !valSigs =  b > 0 && b < c

-- calculating probalilities for the data r_n.
r_n :: Mmodel -> Double -> Double -> Double
r_n    m  mu  x                     =  data_given_model  / p_data
     where       data_given_model   =  alpha * nxmu
                 p_data             =  alpha * nxmu + (1 - alpha) * nxmuE
                 !alpha             =  alph m
                 !nxmu              =  normal x mu (sigD m)
                 !nxmuE             =  normal x mu (sigE m)

-------- weighted sums for the emAlgorithm
rn_mean'' ::  Predicted  -> RNs -> ((Double, Double), (Double, Double))
rn_mean''  ps rns =  ( (dataMean , errMean) , (nd, ne) )
             where
                   ((nd, ne),(sumW, sumWE))  =  foldl'Rnf  fF ((0,0),(0,0)) pdrs -- $ zipVector (,)  ps  rns
                   mkD (a,r) (x, y)   =  (x + r  , y + (1 - r) )
                   mkN (a,r) (x, y)  =  (a + x, (1 - a) + y) -- (1 - r)
                   fF (b,b1) a       =  (mkD a b, mkN a b1)
                   pdrs              =  zip (toList ps) (toList rns)
                   --
                   mkMn f n p  b     =   p * (f b) /n
                   nn                =  nd + ne
                   dataMean          = nd / nn
                   errMean           = ne / nn 

-- variances: ran_vars takes the result of rn_mean', which returns the
-- totals for the casses and the weighted sums
--rn_vars :: Mmodel   -> ((Double, Double), (Double, Double)) -> Data -> RNs  -> (Double, Double)
rn_vars  nD nE rns ps ds   =  (fst squares / nD , snd squares / nE )
            where squares    =  foldl'Rnf  fF (0, 0) (zip rns (zip (toList ps) (toList ds)))
                  fF (a,a1) (r ,(p,d)) =  (a + r * (p - d)^2 , a1 + (1-r) *(p - d)^2)

---------------------------------------------------------------------------------------------------------------
--- calculate the mixture model: takes a model and the number of cases, n
--------------------------------------------------------------------------------------------------------------
ml_alpha  ::  Mmodel -> Double -> Double
ml_alpha     ms      n             =  0.5 *  log n  - (n_alpha + 0.5) * log alpHa - ml_alpha_etc
            where    !ml_alpha_etc  =  (n - n_alpha + 0.5) * log (1 - alpHa) - 0.742
                     !n_alpha       =  alpHa * (n + 1)  - 0.5
                     !alpHa         =  alph ms

mmlSgAlpha :: Mmodel ->  RNs -> Double
mmlSgAlpha     m      rns  =  foldVector sF 0 rns
    where
        sF !a !r   =  let vr = val r in
                        a + vr * log  (vr / alpHa)  +  (1 - vr) * log  ((1 - vr) / alpH1)
        !alpHa     =  alph m
        !alpH1     =  1 - alph m
        val  a  | a >= 0 && a <= 1 = a
                | a < 0  = 0.0000000001
                | otherwise = 0.9999999999

----
mmlMixture :: Mmodel ->  RNs -> Double
mmlMixture      ms    rns    =     mla + mlSa
              where  n      = fromIntegral $ dim rns
                     !mla    = ml_alpha ms n
                     !mlSa   = mmlSgAlpha ms rns

------------------------------------------------------------------------------------------------
-----              learning the RPR
------------------------------------------------------------------------------------------------

-- a type for models and their message lengths
data OModel = Omod !Mmodel !Double  --deriving (Show)


-- averaging the oMods
oMod_mean    []   =  Nothing
alpha_mean (x:xs) =  Just (mDiv n ms)
  where alpha_mean_aux y ys = foldl'Rnf  aVg (1,y) ys
        aVg (!n, Omod (MkModel a b c ds  t) ml) (Omod (MkModel a1 b1 c1 ds1  t1) ml1) =
                   (n+1 , Omod (MkModel (a+a1) (b+b1) (c+c1) (zipVectorWith (+) ds  ds1) (t+t1)) (ml+ml1) )
        (n, ms) = alpha_mean_aux x xs
        mDiv n (Omod (MkModel a b c ds  t) ml) =
               Omod (MkModel (a/n) (b/n) (c/n) (mapVector (/n) ds) (t/n)) (ml/n)


instance Ord OModel where
   Omod _ ds <= Omod _ ds2  =   ds <= ds2
--
instance Eq OModel where
    Omod _ ds == Omod _ ds2  =  ds == ds2

--mkOmod  ms  ds  = Omod ms ds

oMod (Omod m _ ) = m
oModMl (Omod _ ml) = ml

--- valid OMod
validOM :: OModel -> Bool
validOM  (Omod m mgl)  =  noNan mgl && valid m
         where noNan a =   a > 0 - 10000000 &&  a < 10000000


--------------------------------------------------------------------------------------------
-- learn the parameters around the predicted value using the em algorithm
--nextModelR :: Predicted -> Data -> Vector Double -> Mmodel -> Mmodel
nextModelR ::  Data -> Predicted -> RNs -> Mmodel -> Mmodel
nextModelR ds ps rns m   =   MkModel aa1 bb cc1 (aBar m) (tau m) -- newMod, mixture, newRns)
                  where
                  ----------- quasi EM algoritm -------------------
                     !aa     =  (nD  + 0.5) / (nD + nE + 1)
                     aa1     =  if aa <  0.55 then (7 / 9.7) else aa -- then 1 - aa else aa
                     !bb     =  sqrt vd -- sd  of the data class
                     !cc     =  sqrt ve -- sd  of the error class
                     !cc1    =  max cc (bb * 1.5)
                     -- the new parametes are determined by values of the updated predictions
                     (!vd,!ve) =   rn_vars nD nE (toList rns) ps ds
                     !m1      =   rn_mean'' ps rns --
                     !nD      =   fst (snd m1)
                     !nE      =   snd (snd m1)

--- EM of sorts. Ideally, should iterate until the parameters are equal
-- nextModelR' :: Bool -> Matrix Double -> Vector Double ->  Mmodel
--nextModelR' :: Predicted -> Vector Double -> Vector Double -> Mmodel -> (Mmodel, Double)
--nextModelR' ps ys m  = nextModelR'' (invSlopes ps) ys m -- plst `pseq` (minimumBy (comparing snd)  plst) --
-- -- ys
{-# NOINLINE nextModelR' #-}
nextModelR' :: (RandomGen g) => g -> [Int] -> Maybe Bool -> Matrix Double -> Vector Double ->  Mmodel -> IO (Mmodel, Double)--
nextModelR' g cts restrictSlopes matrix ys inMod =  do
           let ks = (plst1 gg)
           ks `pseq` (return . ssd . (!! 0) $ ks)
    where
        (gg,_)      = split g
        ssd (_,b,c) = (b,c)
        trd (_,b,c) = c
        --
        ps          =  invS restrictSlopes . (matrix <> ) . aBar --
        ps1         =   (matrix <> ) . aBar
        --------------------------------------------------------------------------------------------
        ----- updating the errors 
        rns m       =   zipVectorWith (r_n m) (ps m) ys
        faA         =  fromIntegral . (\ a -> if a == 2 then 1 else a `div` 2)  . dim -- . aBar
        --------------------------------------------------------------------------------------------
        applyMM g   = iterate  (unsafePerformIO . gMod gg) (1 , inMod , mmlMixture inMod (rns inMod) )
        plst1  g'   =  [ maybe ks id (listToMaybe xs)  | 
                          let ys = myTake 35  . takeWhile (\(_,md,_) -> sigD md >= 0.01) $ applyMM g'
                        , let (front, ks) = (init ys , last ys)
                        , let xs  = [fromJust y | let y = similarBy simEqML  front, isJust y]
                        ]
        --- compare the message lengths  ---
        simEqML :: (a, Mmodel, Double) -> (a, Mmodel, Double) -> Bool
        simEqML (_, a ,mla) (_,b ,mlb)  =  abs (mla - mlb)  < 0.00001 -- sim_eq a b --
        ---

        gMod :: (RandomGen g) => g -> (Int, Mmodel, Double) ->  IO (Int , Mmodel, Double)
        gMod g (n , md, _)   =  do
            -- print ("iteration: " ++ show n ) -- ", probabilities: " ++ (show . (!! 8) . toList $ rns md) ++
            let dms mx = (rows mx, cols mx)
            -- print ("matrix dims: " ++ show (dms matrix) ++ " ; wWW dim: " ++ show (dms wWW))
            --       " alpha: " ++ show (alph md) ++ " std: " ++ show (sigD md)  )
            -- print ("num linear parms: " ++  show cts )
            let points = "\ndata points: "++ (printElm $ toList ys )
            let predictions = "\n predictions : "++ (printElm . toList $ ps md )
            let otherPredictions = "\n predictions : "++ (show . findCuts . toList $ ps1 md )
            let errors = "\nerrors : "++ (printElm . toList $ rns md )
            let dataStd = "\nData std  : "++ (show $ sigD md )
            let errorStd = "\nError std  : "++ (show $ sigE md )
            let abund = "\nabundance  : "++ (show $ alph md )
            let iter n' = "\n---- \niteration: " ++ show n' ++ "\n-----"
            let output n' = L.pack ((iter n') ++ points ++ predictions ++ errors ++ dataStd ++ errorStd ++ abund)
            let file = "probs1.txt"
            let mixR = mmlMixture nmd rnN
            return (n+1 , nmd,  mmlMixture nmd rnN ) 
           -- | otherwise =  (nmdd, mmlMixture nmdd rnN')

            where
                ------ restart with the original model if there is an invallid model ----
                md1  =   md 
                ---------------------------------------------------------------------------
                !md' =  nextModelR ys (ps md1) (rns md1) md1
                nmd  = nextModelR ys (ps mdd) (rns mdd) mdd
                rnN  = rns nmd
                ------------ updating the linear parameters ---
                siG  =  sigD md' -- model
                siGE =  sigE md' -- model
                abar =  aBar md'
                rN   =  rns md'
                lnR  =  dim rN
                taU  =  tau md'
                ------
                wWW  =   diagRect 0  rN lnR lnR
                xTW  =  (trans matrix) <> wWW
                xTWx =  xTW <> matrix
                ---------------re calcuate the model with the new parameters ----------------------
                pli b (i,j) v = if i == j then v  else v -- + b
                zZ   =  mapMatrixWithIndex  (pli (1/taU^2)) xTWx
                --
                arN_aux =   head . toColumns . linearSolveLS zZ $ asColumn (xTW <> ys)
                arN  =   arN_aux
                ---
                nn   = faA arN
                tuN  =  sqrt ((arN <.> arN) /(nn + 1))
                mdd  =  MkModel (alph md') (sigD md') (sigE md') arN tuN

-------- inverting negative slopes --------------------------
invNeg :: Vector Double -> Vector Double
invNeg acs  = zipVectorWith flipSlp acs (fromList [1 .. dim acs] :: Vector Int)
    where
        flipSlp y a    = if even a && y < 0 then  invS1 y else y
        invS1 n
           | n >= -1   =  n *  (-0.4)
           | otherwise =  (- 0.6) / n

-- inverting the predictions
invS nSlp  =
    case nSlp  of
        Just tf ->  fromList . head  . manyInvNS tf . toList
        anyElse -> id


----------   INITIALIZE A MODEL
-----------------------------------------------------------------------------------------------
initModelR  gen acs       = initModelGD  gen (dim acs)

----
initModel  gen s acss    = MkModel alpha sgD sgE acs taU
            where
                   [alpha, kk]  =   take 2 (randomRs (0.6, 0.9) gen)
                   sgD    =   max 0.2 s -- (0.7 * s)   -- minimum values -- (values !! 0) (values !! 2)
                   sgE    =  65 * kk * sgD -- maximum  values
                   acs    =  invNeg acss --  acs -- invNeg acs
                   taU    =  vstDiv  acs

---- for reinitializing the mizture model
initModelMix  gen aA acs   = MkModel alpha sgD sgE asc1 taU
            where  --(g1, _)  =  split gen
                   sgD    =   head (randomRs (0.15, 4.5) gen)
                   alpha  =   head (randomRs (0.68, 0.95) gen)
                   --sgD     =  minimum values -- (values !! 0) (values !! 2)
                   sgE    =  4 * sgD -- maximum  values
                   asc1   =   invNeg acs
                   taU    =   vstDiv  asc1 -- sqrt ((asc1 <.> asc1) /(aA + 2)) -- vstDiv  asc1
                   ----

--- initializing the model for gradient decent
initModelGD  gen abarLen    = MkModel alpha (min sgD sgE) (max sgD sgE) abar taU
            where  (g1, _)  =  split gen
                   values  =  myTake 3 (randomRs (1, 50.0) gen)
                   alpha   =  (myTake 1 (randomRs (65.0, 95.0) gen) !! 0)/100
                   sgD     =  min (values !! 1) (values !! 2)
                   sgE     =  max (values !! 1) (values !! 2)
                   taU     =  vstDiv  abar -- values !! 3
                   abar    =  fromList (myTake abarLen  [ x | (y,a) <- zip (randomRs (-10, 20) gen) [1 .. ]
                                                              , let x =  if even a && a >= 0 then y else (-1) * y  ] )


------------------------------------------------------------------------------------------------
--- calculating probability values
filterV :: (Double -> Bool) -> Vector Double -> Vector Double
filterV  f  va  = initV $ foldVector (\a va -> if f a then vcons a va else va) dummy va
    where vcons x xs = join [fromList [x],xs]
          dummy      = fromList [-10] :: Vector Double
          initV vs   = if n > 1 then subVector 0 (n-1) vs else dummy
               where n = dim vs

-- error and data
-- Vector of Points -> Vector of means -> alpha -> (a -> Bool)
dataError :: Mmodel -> Vector Double -> Vector Double -> (Vector Double,Vector Double)
dataError  mod actual  predicted =  (mkProbs (>= am)  , mkProbs (< am) )
    where
        --mn                 =
        am                 =  let a = alph mod in a  -- - (0.05 * a)
        --am'                = let a = alph mod in a + (0.05 * a)
        mkProbs    f       =  zipVectorWith (\a b -> if f (calProbs mod a b) then a else -10) actual predicted
        calProbs mod x mu  = let dataProb = am * normal x mu (sigD mod)
                                 errorProb = (1- am) * normal x mu (sigE mod)
                             in
                                 dataProb / (dataProb + errorProb)

instance Show Mmodel where
    show (MkModel a b c ds ss)  = "\nalp:"++show a ++  "\nstd:" ++ rest1
             where lparms e e1  = "\nabr:" ++  printElm e  ++ "\ntau:" ++ show e1
                   rest1        = show b ++ "\nste:" ++ show c ++ lparms (toList ds) ss -- ++ "\n"
instance Show OModel where
   show (Omod m ml) = show m -- ++ "mln:" ++ show ml
-------------------------------------------------------------------------------------------------
