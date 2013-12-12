{-# LANGUAGE BangPatterns #-}
--{-# XRankNTypes #-}

module RprGD2 (OptimisationType (Rapid , FromData ,WithDepth)
    , mmlRprGdEvl,mml2DsGD -- ,mmlRprTest
    , mml2DsGen, RprMix2.aBar,mapP --  , stdTest
    --,mmlRprTest
    ) where

import Numeric.LinearAlgebra
import Numeric.GSL.Statistics (stddev,lag1auto)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile, pack, unpack, intercalate,appendFile, append,concat)
import System.Random
import Control.Concurrent
import Control.DeepSeq
import Foreign.Storable
import Control.Parallel (par, pseq)
import qualified Control.Parallel.Strategies as St-- (parMap)
import System.IO.Unsafe
import Data.Maybe (isJust,fromMaybe,maybe,listToMaybe,fromJust)
import Data.Time
import Data.Char (isDigit)
--import Text.Printf
import Data.Ord (comparing)
import Control.Monad (liftM,liftM2)
import Data.List (transpose, foldr1, sort, foldl' ,nub, nubBy,sortBy,  minimumBy, partition)
import qualified Data.Map  as M (Map,empty,insertWith' ,mapWithKey,filterWithKey,toList, fromList)
--import IO
--------------------------------------------------------------------------------------------
import HMDIFPrelude (bsReadChr,bs2Int')
import RprMix2 -- (vLength)
import LINreg2
import ResultTypes
import ProcessFile (writeToFile,log2File,writeOrAppend,myTime)
import LMisc hiding (force) 
import ListStats (nCr,normal,binomial ,myTake,-- corr,
                  mean, adjustedMatrix,cdf, meanBy, stDivBy, -- stDiv,
                  adjustedMatrixN, adjustedMatrixPN,stNorm,mean,stDivM) -- gammaln,
---------------------------------------------------------------------------------------------
----                 optimisation type
data OptimisationType = Rapid | FromData | WithDepth Int deriving (Show, Eq)-- | WithNum Int
---------------------------------------------------------------------------------------------
---- Paralell definitions
concatPMap ::  (a -> [b]) -> [a] -> [b]
concatPMap f = concat . St.parMap (St.parList St.rseq) f

-- parallel map and parallel concatMap
--mapP  = St.parMap (St.dot St.rseq St.rpar)

mapP' :: (a -> b) -> [a] -> [b]
mapP' f = foldl' (\bs a -> let fa = (f a) `St.using`  St.rseq in
                                fa : bs ) []
--
mapP :: (a -> b) -> [a] -> [b]
mapP  f = St.parMap St.rseq f 

concatPMap' f   = foldl'Rnf  (\xs x -> xs ++ [St.using (f x) (St.dot St.rseq St.rpar) ] ) []

--- strict fold vector
-- foldVector :: Storable a => (a -> b -> b) -> b -> Vector a -> b
foldVector' :: (NFData b, Storable a) => (b -> a -> b) -> b -> Vector a -> b
foldVector' f a vs = foldVector (\b g x -> g (seqit $ f x b)) id vs a
    where seqit a = rnf a `pseq` a
--------------------------------------------------------------------------------------------------
myMinimum :: (NFData a, Ord a) => [a] -> a
myMinimum   (x:xs) = foldl'Rnf  min x xs
myMinimum     _    = error "error: myMinimum empty list"
--
--mmlRprGD :: Vector Double -> Double -> Double -> Matrix Double -> Mmodel -> (Mmodel, Double)
{-# NOINLINE mmlRprGD #-}
mmlRprGD g ys n cts nSLP dss (ml, model) = mmlRprGD_aux g ys n cts nSLP dss  model

mmlRprGD_aux :: (RandomGen g) => g
     -> Vector Double
     -> Double
     -> [Int] 
     -> Maybe Bool
     -> Matrix Double
     -> Mmodel
     -> IO OModel 
mmlRprGD_aux g ys n cts nSLP dss model =  do
    return $ Omod (nAbaMod model taU) oldMsgLn
        where
            ------- compute values from the input model, eg the weighs-----------
            nAbaMod  (MkModel a b c x _ )  k  =  (MkModel a b c x k)
            ----
            fL             = (+ 1) . fromIntegral . length
            aA'            = fL cts
            -------------------------------------------------------
            siG            =  sigD  model 
            siGE           =  sigE  model 
            taU            =  tau   model   
            ---------------------------------------------------------
            xXa            =  invS nSLP (dss <> (aBar model)) ---
            abar           = aBar model

            aA             =  fromIntegral . (\ a -> if a == 2 then 1 else a `div` 2)  $ dim  abar
            errFun  a b    =  (r_n model a b) 
            rN             =   zipVectorWith  errFun xXa ys -- (r_n model)
            lnR            =   dim rN   -- get the magnitude of rN
            wData :: Matrix Double
            wData          =  diagRect 0 rN lnR lnR
            wError :: Matrix Double
            wError         =  diagRect 0 (mapVector (1 -) rN) lnR lnR
            ---
            xtWx            =  ((trans dss) <> wData) <> dss
            ------------------------------------------------------------
            ---
            nAlpha         =  foldVector (+) 0 rN  
            nAlpA1         =  nAlpha - aA - 1 --
            --
            ysSubXa        =  zipVectorWith (-) ys  xXa -- the data minus the predictions
            ySubWySub ww   =  ysSubXa <.> (ww <> ysSubXa) -- y
            --
            dehAbarSq      = (abar <.> abar)
            aBarSqTau1     =   dehAbarSq / (2 * taU^2)
            ----------------------------------------------------------------------------------------
            ------------------- calculate the message length of the input model --------------------
            ----------------------------------------------------------------------------------------
            !oldMsgLn     =   l1 `par` l2 `par` l3 `par` l4 `pseq`   (l1 + l2 + l3 + l4)
            ----------------------------------log 0.01 *------------------------------------------------------
            restrPr       = ((aA + 1)* log 2)/ 2
            restrPrior     = maybe restrPr (\_ -> restrPr) nSLP
            l1             =   (aA + 2.0) * logg taU + aBarSqTau1 - restrPrior  + 0.5 * ((n-2) * log (2 * pi))
            ----------------------------------- log 2 * (aA + 1)/2 ---------------------------------
            l2              =   nAlpA1 * logg siG + (1/(2 * siG^2)) * (ySubWySub wData)
            l3              =  (n - nAlpha) * logg siGE + (1/(2 * siGE^2)) * (ySubWySub wError)  + 0.5 * logg (2 * nAlpha)
            mlLogs          = 0.5 * log (det xtWx) + 0.5 * logg (2*(n - nAlpha)) +  0.5 * log (pi * (aA + 3)) 
            l4              =   mlLogs - 1.319
----------------------------------------------------------------------------------------------------
dummyMod       = Omod (MkModel 0 0 0 (fromList [1]) 0) 100000000
vunStdNorm' vs = vunStdNorm (vmean vs) (vstDiv vs) vs
{----------------------------------------------------------------------------------------------------
mmlRprGdEvl: the mmlRPR  evaluation function. Takes:
n  - the range in years of roughness data (which corresponds to the number of data points
      when dealing with simulation data)
ms - the number of chains that are joined together - the maintenance strategy.
     Note that the mml2DS uses the mmlRPR on adjaent sections joined together. Hence this
     parameter says how many sections are to be joined for calls to mmlRprGdEvl. When this
     functions is called outside of the mml2DS, this valus is always 1 (i.e we do not make any
     prior assumptions about maintenances on the chain)
ys  - a lsit of the data values for this chain
pp  - the interval in years between maintenenace interventions
gen - a random generator to initialize the process
nSlopes - determines whether to exclude negative slopes: if true, negative slopes are excluded
           otherwise they are not
rprOnly - toggles the applicaion of the MMLRPR function only. The likelihood of maintenance
          is not applied to each interventions
----------------------------------------------------------------------------------------------------}
mmlRprGdEvl :: (RandomGen g) => Int ->
    Int -> -- error search depth limit
    [Vector Double]  -> -- Either (Vector Double) [Double] -> -- the data in standardized for
    Double ->
    g ->
    Maybe Bool -> -- negative slopes (temporarily used for turning on and off the mixture model)
    Int ->
    IO (Maybe ( (OModel,(Vector Double,[Int]) ) , [Double])) --ms
mmlRprGdEvl n dpt ys pp gen nSlopes mtd = -- mN std
    mmlRprAuxGD n dpt m p (filter ((== mLen) . dim) ys) pp gen nSlopes mtd True -- ms -- mN std
        where
            p     =  1 / pp
            --- don need this claculation. we can filter out irelevant values
            m     =  n `div` mtd
            mLen  = maybe 0 (\_ -> maximum $ map dim ys) (listToMaybe ys)
            ---------------------------------------------------------------------------------------------------
            unStd :: (OModel,(Vector Double,[Int])) -> (OModel,(Vector Double,[Int]))
            unStd  (Omod (MkModel c b e ks d)  ml,(vs,ys)) =  (Omod (MkModel c b e (vunStdNorm' ks) d)  ml,(vunStdNorm' vs,ys))
            unSd ms =
                case ms of
                    Nothing -> Nothing
                    Just (a,b) -> Just (unStd a, b)

------------
mmlRprAuxGD n _ m p ysv pp gen nSlopes mtd toOpt = do
    return minMod
    where
        minMod = liftM applyMixture $
                    maybe Nothing (\_ -> Just $ minimumBy (comparing (fst . fst))  rprs1) (listToMaybe rprs1)
        --
        result  rprss =  (snd (minimumBy (comparing (fst . snd)) rprss), map fst rprss) 
        f         =  fromIntegral
        (g1,_)    =  split gen
        abr' ::  Matrix Double -> [(Double, (Vector Double, (Double, Vector Double))) ]
        abr'      = (: []) . minimumBy (comparing fst)
                    . zipWith (\f a -> f a) (map (\ys -> mmlLinearRegLP ys 1 1) setLen)
                    . replicate len -- (length ysv)
            where
                len    = length ysv
                setLen = replicate len $ join ysv
        ----------------------------------------------------------------------------
        initM b s =  initModel gen s b -
        initLz (ml, (prd, (s,ab))) =  (ml, (prd , initM ab s))
        --
        mod :: Matrix Double -> [(Double, (Vector Double, Mmodel))]
        mod  mxx  =  map   initLz $ abr'  mxx 
        ----------------------------------------------------------------------------
        nN = f . sum $ map dim ysv -- n
        nNs = map ((\n -> [1 .. n]) . f . dim) ysv
        fLn = (+ 1) . f . length
        msgLm (Omod _ l) = l
        appMML g1 nM ct  mmX (ml,(prd , inMd))  = unsafePerformIO $ mmlRprGD g1 (join  ysv) nM ct nSlopes mmX (ml, inMd)
        iTr (mX, cts) = (map (appMML gen nN cts  mX) (mod mX), (mX, cts))
        cMML  = mapP (getPredictions nSlopes) . mapSnd . iTr
        nps cmm  =   realignedNpieces cmm   mtd
        --------------------------------------------------------------------------------------------
        ----------- applying the mixture model to the discovered intervention, only ----------------
        rprs1  =   [ (kx, cm1) | cm1 <- [0 .. m],  kx <- ( concatPMap (mapSnd . iTr) . (nps cm1)) nNs  ]  `St.using` (St.parList St.rseq)
        applyMixture ( (Omod md mln , (mx , cts)), cm) =
            let   (!nMd , !mxL)  = unsafePerformIO $ nextModelR' gen cts nSlopes mx (join ysv) md
                  ncma    = n
                  nncma   =  ncma `nCr` cm
                  ncr     =   (binomial n cm p) / f nncma -- else 1
                  lncr    =   log ncr
                  newOMod = Omod nMd ((lncr + mln) + mxL)
                  prds    = getPredictions nSlopes (newOMod, (mx, cts))
            in    (prds , [lncr]  )
        --------------   end applying the Mixture Model at the end ----------------------------------
        getPredictions nSlp (mm, (xConfig, cts))  =   (mm , (invS nSlp  prds , cts)) 
            where
               prds     =   xConfig <> (aBar (oMod mm))
        --- retain the domain of the function
        retDom :: (a -> [b]) -> a -> [(a , b)]
        retDom  f  a  =   map (\x -> (a, x)) ( f a)
        ---
        nAbaMod  (MkModel a b c _ x)  k  =  (MkModel a b c k x)

{----------------------------------------------------------------------------------------------------
  The mml2DsGD functions applies the MMLRPR to alogorithm to a list of data
  points (i.e. the readings or chainages) and after
  sectioning them off mml2DsGD years distance gen [[Double]]
--}
--- returning results
mml2DsGen :: (RandomGen g) => g -> TdsParms ->
    OptimisationType -> -- the optimisation type (Rapid , FromData ,WithDepth)
    SecOrSchm ->
    IO SecOrSchm
mml2DsGen g tprms optType ssm =
    case ssm of
        -- analysis results for sections
        Left  sect -> do
            let (a, b , c ,d) = (sCondition sect, sScannerCode sect, sScannerSorrg sect, sCWay sect)
            let mkSect = Section a b c d (sChains sect)
            liftM (Left . mkSect . omd2Results) $ mml2DsGD g tprms optType [xs | xs <- sChains sect, length (cData xs) > 1]
        -- results for schemes
        Right schm -> do
            nScmRecs <- mapM (mkSchemeRecRes g tprms optType) (schmRecs schm)
            return $ Right $ Scheme (schemeName schm) (schmCondition schm) nScmRecs
    where
        mkSchemeRecRes :: (RandomGen g) => g ->
            TdsParms ->
            OptimisationType ->
            SchemeRec ->
            IO SchemeRec
        mkSchemeRecRes g tp opT src = liftM (mkSchmRec . omd2Results) $ mml2DsGD g tp opT chn
            where
                mkSchmRec = SchemeRec scd srd srnm srg stc sec chn
                --
                scd = scmScannerCode src
                srd = roadClassCode src
                srnm = roadName  src
                srg  = scmScannerSorrg  src
                --- carriage way
                stc = startChain   src
                sec = endChain  src
                chn = scmChains src


--}
-- returning a OMod, etc
mml2DsGD :: (RandomGen g) => g ->
    TdsParms -> -- the parameters for the mmlTds
    OptimisationType -> -- the optimisation type (Rapid , FromData ,WithDepth)
    [Chain] -> --- a list of all the data for each chain in a section or scheme
    IO ([(OModel, ([Double], [Int]))], [Int])
mml2DsGD  gen tdPrs optType chxss = do
    ---------------------------------------------
    scrollLog ("Identifying candidate groups for section with length: " ++ show len) -- (max 3 (len `div` 15))
    !mks1  <-   (joinPairsV dist optType 2 len 25 xss)
    ------
    let total = length mks1
    let msgP1 = "Fitting progression rate and identifying outliers for section length: " ++ show len ++". "
    let msgP2 = show total ++": candidate groups found."
    onlycuts <- liftM (maybe 0 (fromJust . bs2Int') . listToMaybe . head) $ bsReadChr  ',' "./mmlTdsFiles/onlycuts.txt"
    -- print ("negslopes is : " ++ show nSlopes)
    if len > 0 then
        if null mks1 then  do
            let noGrupMsg = "NO Groups for section length: " ++ show len
            logLog ("-------------------"++noGrupMsg)
            return  dummy
        else   if onlycuts == 1 then do
            logLog ("only first cuts printed")
            return dummy
        else do
            --
            scrollLog  (msgP1 ++ msgP2)
            let mks2  = [(a,b,c) | ((a,b),c) <- zip (zip mks1 (rGens gen)) [1 ..]]
            let applyRpr = mapP (mml2DsGD_aux1 total minTrend scrollLog)
            let appMin  = liftM (minimumBy compML'')  . sequence .  applyRpr  -- ($!)
            appMin mks2
    else do
        logLog ("Empty section list sent to MML2DS: " ++ show len ++". limit: "++ show lenLim)
        return dummy
    where
        ---- projections --
        xss                  =  map cData chxss
        (!dpt, !pp , !dist)  =  (searchDept tdPrs, rngInYrs tdPrs, rngInDist tdPrs)
        (nSlopes,lim,minAnl) =  (negSlopes tdPrs, maxChn2Aln tdPrs, minChn2Aln tdPrs)
        minTrend             =  minPts2Trd tdPrs
        loggers              =  logFun tdPrs
        scrollLog            =  maybe (\_ -> return ())  (\_ -> loggers !! 0) (listToMaybe loggers)
        logLog               =  maybe (\_ -> return ())  (\_ -> loggers !! 1) (listToMaybe loggers)
        maxChain             =  (foldl' (\n xs -> max (length xs) n) 0 xss)
        lenLim               =  lim * maxChain 
        len                  =  length xss
        minLen               =  minAnl * maxChain
        -------------------------------- end of projections ------------------
        dummy                =  ([(dummyMod , ([0] ,[0]))] ,[])
        pm                   =  1 / dist
        compML''             =  comparing  (sumBy (oModMl . fst). fst)
        f                    =  fromIntegral
        {--------------------------- logging the output from the meddage lengths --------------------
        printCutsInfo :: OptimisationType -> ([(OModel, ([Double], [Int]))], [Int]  ) -> IO ()
        printCutsInfo opTp (ks,cs) = do
            let cutsMsg = "\n cuts: "++ printElm cs
            let mlgMsg  = "; \t message length: " ++ (show $ sumBy (oModMl . fst) ks)
            let name = case opTp of
                            Rapid         ->  "fromData" -- "priorOnly"
                            FromData      ->  "fromData"
                            WithDepth _   ->   "fixedDepth"
            let file = "./SimulationFiles/"++name++"groups.txt"
            -- let header = "\n-----------\t cuts and message lengths ------------\n"
            let output = L.pack (cutsMsg ++ mlgMsg)
            writeOrAppend file output
        ---------------------------------------------------------------------------------------}
        mml2DsGD_aux1 :: RandomGen g =>  Int ->
                           Int ->
                         (String -> IO()) ->
                         ( ([([Vector Double], Int)],[Int]), g, Int)  ->
                         IO ([(OModel, ([Double], [Int]))], [Int])
        mml2DsGD_aux1 total minToTrend write ((pgroups,pgcuts),gen,num)  =  do
            write ("Calculating mmlrpr on group: "++ show num ++ " of " ++ show total)
            let max1  = sumBy snd pgroups -- sum pgcut
            ----
            let  calc = [ (result,pgcuts) | 
                            let !rprsCalcs =  unsafePerformIO . sequence $ mapP (calcRpR max1 gen minToTrend) pgroups -- do the mlrpr on each grou
                            , all isJust rprsCalcs -- .  filter isJust
                            , let result   = (map fromJust . filter isJust) rprsCalcs]
            if null calc then do
                write ("Invalid mmlrpr group for group "++ show num)
                return dummy

            else do
                retMinComp calc
                where
                    --
                    retMinComp =  return . minimumBy  compML''
                    --
                    calcRpR :: RandomGen g => Int -> g -> Int ->
                               ([Vector Double], Int) -> IO (Maybe (OModel,([Double],[Int]))) -- , [Double]) -
                    calcRpR ns g mtd (vys, ms) = do
                        !rprs <- mmlRprGdEvl ms dpt vys pp g nSlopes mtd
                        case rprs of
                            Just ( (Omod r1 r2, prs),nn) -> do
                                       let smm = ncr' + r2
                                       let str = ("\nncr' is: "++ show ncr' ++" mLen is: "++ show r2 ++". Their sum is: " ++ show smm)
                                       -- L.appendFile "tempLog1.txt" (L.pack str)
                                       return $ Just (Omod r1 smm , (toList (fst prs), snd prs) ) --
                            Nothing ->   return $ Nothing
                        where
                            ncr'  =   (binomial ns ms pm) / f (ns `nCr` ms)
-----------------------------------------------------------------------------------------------------}
------------
--------------------------------------------------------------------------------------------------
findGrpsFromData :: Double -> -- the distance of intervention points
    Int              ->  -- lenght of the input list
    [[Double]] ->           -- ^ the data to aling
    IO [([([Vector Double], Int)], [Int])]
findGrpsFromData dist inLen   =
    return . findCandidateSet .  uncurry calculateFixedGroups1 . findInterventions_aux
    where
        findCandidateSet xs
            | length xs > 100 = maintainBy' avgRelCorr (Just 45) xs
            | otherwise       = (take 10 .  sortBy (comparing avgRelCorr )) xs
        --
        avgRelCorr    =  (meanBy (relativeCorr . fst)) . fst
        -- find possible interventions from the data
        findInterventions_aux ::  [[Double]] ->    ([[Double]] , [[Int]])
        findInterventions_aux  xys =   (xys ,  (nub . findGrps1 0.7) xys)
            where
                findGrps1 tolerance xs =  [ ys | let stDiv = stddev . fromList
                                                , ys <-  findGrps tolerance 38 mmlCorr xs
                                          ] `St.using` (St.parList St.rseq)
        -- calculate fixed groups
        calculateFixedGroups1 :: [[Double]] ->  [[Int]]  -> [([([Vector Double], Int)], [Int])]
        calculateFixedGroups1  xys  =  map (unzip . foldr mkLst [] . filter (not . null . fst) .  mkIntervals vs )
            where
                vs              = (map fromList xys)
                mkLst (ks,k) ps =  ((ks, sumBy dim ks) , k) :  ps
        -- relative correlaton between adjacent chains
        relativeCorr :: [Vector Double] -> Double
        relativeCorr xs
            | length xs < 2 =  4 -- bias against groups with only one chian
            | otherwise     =  mcD xs
            where
                corrDist ps qs =  abs (nCorrV ps qs - 1)
                mcD            =  meanBy (exp . uncurry corrDist) . zipp

----------------------------------------------------------------------------------------------------
--- an infinite list of random generators
rGens :: RandomGen g =>  g -> [g]
rGens g = g : (g1 : rGens g2)
    where (g1, g2) = split g

------------------------------------------------------------------------------------------------------------------------
joinPairsV :: Double -> -- the distance of maintenence interventions
            OptimisationType -> -- the optimisation type (Rapid , FromData ,WithDepth)
            Int ->              -- ^ the minimum number of chains to aligh
            --Int ->            -- ^ maximum
            Int ->              -- ^ length of the input list
            Int ->              -- ^ the maximum number of chains to align
            [[Double]] ->  -- ^ the data to aling
            IO [([([Vector Double], Int)], [Int])]
            --[([(Vector Double, Int, Double )],[Int])]
            -- (joinPairsV pm optType minLen lenLim 3 len lim) xss
joinPairsV dist optType malg xysLen lim xys -- minL maxL
            | null  xys           =  return []
            | xysLen <  4         =  return joinFew -- ' pm --xys
            --- check the length of the combined list
            | lenData < 10        =  return joinFew
            | otherwise           =
                case optType of
                    Rapid         ->  findGrpsFromData dist xysLen xys
                    FromData      ->  findGrpsFromData dist xysLen xys
                    WithDepth d   ->  calculateFixedGroups vxys [(repeat d)]
            where
                lenData           = sumBy length xys
                --
                vxys              =  map fromList xys
                ------------------
                vs                =  join vxys
                ms                =  dim vs
                joinFew           =  [([([vs],dim vs)],[])]

---------------------------------------------------------------------------------

-- given a list of possible cuts and the list of chains, we generate the a list
-- of possible maintenance groups defeind by the cuts
calculateFixedGroups :: [Vector Double] ->  [[Int]]  -> IO [([([Vector Double], Int)], [Int])]
calculateFixedGroups  vs  = return . map (unzip . foldr mkLst [] . filter (not . null . fst) . mkIntervals vs)
    where
       mkLst (ks,k) ps =  ((ks, sumBy dim ks) , k) :  ps
       -- return [ (mkIntervals vs cs , cs)  | cs <- ks]
