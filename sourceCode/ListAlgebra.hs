{-# LANGUAGE BangPatterns #-}
module ListAlgebra (square, mkNxN, mkNxN0, leadDiag, (<***>), (<->), (<--->),
                    norm, trace, remN, (<+>), (<.>), (<!.>), (<+!+>), mInverse,  --  zipp,  mult,
                    gaussElim, rowReduce, backSubs, axEqB, isLowerTriangle, --choose,
                    nxmSq, isVec, sDet, det, detL, findDepVecs, sTranspose, Matrix,
       		    module LMisc) where
--
import Prelude
import Numeric
import Data.List (foldl', insertBy, transpose)
import Data.Array
import LMisc



type  Matrix  = [[Double]]

---------------------------------------------------------------------------------------
-- a strict version of transpose
--------------------------------------------------------------------------------------
sTranspose :: Matrix  -> Matrix
sTranspose   []      =    []
sTranspose  ([]:xss) =   sTranspose xss
sTranspose  ((x:xs):xss)  =   (x: [h | (h:_) <- xss ]) : (sTranspose (xs : [t | (_:t) <- xss]))
		where sCons ys yss  =  ($!) (ys:) yss

--------------------------------------------------------------------------------
--- square: says if a matrix is a square matrix
square_aux :: [[a]]  -> Int -> (Bool,Int)
square_aux      (x:xs)     n | neq       =  (neq && tt, 1 + m)
                             | otherwise =  (False, 0)
                            where
                                (tt, m)  = square_aux xs n
                                neq      =  length x == n
square_aux        _        _             =  (True, 1)

square :: [[a]]  -> Bool
square    (x:xs)        =   tt && (n == m)
            where
                n       = length x
                (tt, m) = square_aux xs n

square      _           =  False
---------------------------------------------------------------------------------
-- minor matricse, etc
---------------------------------------------------------------------------------
--- remove the nTh emelent of a list
remN  n  xs                     =   [ x | (x,i) <- zip xs [1..], i /= n]

-- minor of a matris
minor :: Int -> Int -> Matrix   -> Matrix
minor    i     j     xs         =   ($!) (remN i) (map (remN j) xs)

-- returns the element at the ith row and jth column in a matrix
index   i      j     xs         =  (xs !! (i-1)) !! (j - 1)

-- returns the list of all of the ijth minor matrices with given i
minors   i     xs               = [ minor i j xs | j <- [1..l]]
                    where     l = length xs

----------------------------------------------------------------------------------------------
-- matric multiplication, addition, subtraction, etc
--------------------------------------------------------------------------------------------
-- insert x at positin n in xs
insert_at  x   n    xs      =  map fst pairs
                where pairs =  insertBy sF (x,n) (zip xs [1..])
                      sF (_,a) (_,b)  = compare a b -- \a b -> compare (snd a) (snd b))
--mkNxN

mkNxN' :: [a]    -> [a] -> a  -> [[a]]
mkNxN'   (x:xs)  !ys  a =  (ys ++ (x : [a | _ <- xs ])): (mkNxN' xs (a:ys) a)
mkNxN'     []    _   _  =  []

mkNxN    !xs   a  = mkNxN' xs  [] a

mkNxN0   xs  = mkNxN xs 0


--- magnitude of a vector
norm :: Floating a => [a] ->  a
norm                      =  sqrt . foldl' (\a b -> b + a^2) 0
--

--- makes a vector of the leading diagonal in a matrix
leadDiag       xs             =  foldr ldIx [] (zip xs [0..])
       where ldIx (ys ,!n) zs = ys !! n : zs
--
--- the trace of a matrix is the sum of the leading diagonal
trace                          = sum . leadDiag

--- the dot product of two vectors
(<.>) :: Num a => [a] -> [a] -> a
xs      <.>     ys   = foldr (\zs b  -> b + (fst zs * snd zs)) 0 (zip xs ys)


-- multimplying two a n x n matrix -- i.e. W^T W
nxmMult' ::  Matrix -> Matrix -> Matrix
nxmMult'     []        _    =  []
nxmMult'     _        []    =  []
nxmMult'    (x:xs)    ys    =  map (<.> x) ys : nxmMult' xs  ys


--nxmMult xs   ys    =  nxmMult'  xs (transpose ys)


infix 2 <***> -- mutiply two matrices
(<***>) :: Matrix -> Matrix -> Matrix
xs  <***>  ys    | foldr (\list b  -> b && length list == n) True xs  =  nxmMult' xs  (sTranspose ys)
                 | otherwise     =  error " <***> : matrices cannot be multiplied"
                 where         n   =  length ys
--

-- squaring a matrix
nxmSq  xs  =  (sTranspose xs) <***> xs


-- substract two vectors
infix 1  <->
(<->) :: Num a => [a] -> [a] -> [a]
--[]      <->    ys           = ys
--xs      <->    []           = xs
xs      <->    ys             =  zipWith (-) xs ys

-- addition two vectors
infix 1  <+>
(<+>) :: Num a => [a] -> [a] -> [a]
[]      <+>    ys            = ys
xs      <+>    []            = xs
xs      <+>    ys            = zipWith (+) xs ys

--- pre-multiply a n x 1 matrix by a m x n matrix
infix 2 <!.>
(<!.>) ::  Matrix -> [Double] -> [Double]
xs  <!.>    x     =   map (<.>x) xs

-- subtract two n x m matrices
infix 1 <--->
(<--->) :: Matrix -> Matrix -> Matrix
xs    <--->     ys   =   zipWith (<->)  xs  ys

-- add two nxm matrices
infix 1 <+!+>
(<+!+>) :: Matrix -> Matrix -> Matrix
xs    <+!+>     ys   =   zipWith (<+>)  xs  ys

-----------------------------------------------------------------------------------------
-- calculate the  determinant of a matrix using Laplace equation
-- this is very inefficient
----------------------------------------------------------------------------------------
det' ::  Int -> Matrix -> Double
det'   n  [[x,y],[a,b]]         =  x * b - y*a
det'   n     xs                 =  fst det1
      where  det1               =  foldl' c_ij (0,1) (zip (xs !! 0) (minors 1 xs))
             c_ij (!fa, !sa) (!fb, !sb)  =  (fa + (-1)^(n + sa) * fb * det' (n+1) sb , 1 + sa)

--- safe determinanat square matrix
det'' ::  Int -> Matrix  -> Maybe Double
det''  n  [[x,y],[a,b]]         =  Just (x * b - y*a)
det''  n      [[_]]             =  Just 0
det''  n       xs               =  fst det2
      where  det2               =  foldl' c_ij (Just 0,1) (zip (map Just (xs !! 0)) (minors 1 xs))
             c_ij !ass !b          =  (onlyJust (fst ass) (snd ass) (fst b) (det'' (n+1) (snd b)) , 1 + snd ass)
             onlyJust (Just a) m (Just b) (Just c)   =  Just (a + (-1)^ (n+m) * b * c )
             onlyJust     _    _     _       _       =  Nothing

-- unsafe determinanint: Use when the matrix is known to be sqaure
det :: Matrix  -> Double
det xs   |  square xs  =  det' 1  xs
         |  otherwise  =  error "determinant: matrix is not square"
--

-- safe determinaint: Use on any matrix. Returns 'Nothing' if the determinainat
-- cannot be computed
sDet :: Matrix  -> Maybe Double
sDet xs  | square xs = det'' 1  xs
         | otherwise = Nothing
-------------------------------------------------------------------------------------------------
-- determinant using the LU decomposition
detL :: Matrix  -> Double
detL xs   |  square xs        = d  -- * (-1)^k
          |  otherwise        =  error "detL: matrix is not square"
           where  (gElim, k)  =  gaussElim' xs -- . transpose . gaussElim)  xs
                  !d          = (foldl' (*) 1 . leadDiag) gElim
----------------------------------------------------------------------------------------------
-- Inverse of a matrix
----------------------------------------------------------------------------------------------
-- the ijthe cofactor is the determininant of a matrix obtained afcer the ith
-- row and jth column has been removed.
cofactor :: Int ->  Int  -> Matrix  -> Double
cofactor     i    j    xs   | i > 0 && i <= n && j > 0 && j <= n  = detL (minor i j xs)
                            |  otherwise   =   error "Cofactor: invalid coefficients "
                         where  !n  = length xs
--
cofactorMatric_aux  i  n   xs  | i > n      = []
                               | otherwise  = rowCofactor : cofactorMatric_aux  (i+1)  n   xs
                         where !rowCofactor  = map (\k -> (-1)^(i + k) * cofactor i k xs) [1..n]
--
--- calculating the cofactor matrix
cofactorMatrix  :: Matrix  -> Matrix
cofactorMatrix  xs  | square xs    =  cofactorMatric_aux  1  n xs
                    | otherwise    =  error "cofactorMatrix : matrix is not square"
                        where !n    = length xs
--
-- adjoint of a matrix (i.e. the transpose of the cofactor matrix)
adjoint  =  sTranspose . cofactorMatrix


-- the inverse of a matrix using the cofactor method
{--mInverse   xs  = case mInverse' xs of
                  Nothing  -> error "The inverse does not exist"
				  Just a  -> a			
--}
mInverse        xs        =  scalMult (1 / deT) (adjoint xs)
       where
          deT            =  detL xs
          scalMult a xss  =  [ map (*a) xs | xs <- xss]

---------------------------------------------------------------------------------------------
-- gaussian elimination, etc
---------------------------------------------------------------------------------------------
-- finds a vector and the elements in the matrix that are dependent on that vector
---- this naieve version only finds vectors that sum to another vector
findDepVecs :: Num a => [[a]]  -> [([a], [[[a]]])]
findDepVecs    xs   =  finLinCmb_aux xs [] --
--}

--
finLinCmb_aux    []    _     =  []
finLinCmb_aux    (x:xs)  ys  =  case (snd lcmb) of
                                 []  -> finLinCmb_aux xs (x:ys)
                                 _   -> lcmb : finLinCmb_aux xs (x:ys)
                where sumVec =  foldr (<+>) []
                      lcmb   =  (x, [zs | zs <- subs (ys ++xs), length zs > 1, sumVec zs == x])
--
-- pivoting: Note we only pivot to remove columns with o pivot
pivotRows :: Num a => [[a]] -> ([[a]], Int)
pivotRows       (y@(x:_):yss)  | x == 0   =  (pss++[y], 1 + n)
                               | otherwise  =  (y:yss , n)
                        where (pss, n)   =  pivotRows yss
pivotRows        xs                      =  (xs, 0)

--------------------------------------------------------------------------------------------
-- the function gaussElim takes a square matrix and applies  gaussian elimnation
--  (x:xs) is the rol
gaussElim_aux :: [Double] -> Matrix -> Matrix
gaussElim_aux     (x:xs)   ((y:ys):yss)  =  yms : gaussElim_aux (x:xs) yss
				where
				    m    =  y / x
				    ms   =  map (*m) (x:xs)
				    yms  =  (y:ys) <-> ms
gaussElim_aux      _            _        =  []
-------------------------------------------------------------------------------------------------

gaussElim''  xss  =  case pivotRows xss of
                    (xs:yss , m) ->  (xs : (map (0:) gss), m + n)
                        where
                            yss1      = gaussElim_aux xs yss
                            (gss, n)  = gaussElim' (map tail yss1)

-- []         -> ([], m)
gaussElim'  xs@(_:_)  = gaussElim'' xs
gaussElim'  xs  =   (xs , 0)
--
-- onePivot
--onePivot :: Matrix -> (Matrix, Int)
--onePivot   ((x:xs):(y:ys):xss)  | x == 0 && y != 0  = onePivot (y:xs):(x:xs):xss)

gaussElim  = fst . gaussElim'

-- gaussian elimination without appending the 0
--- used for calculating the dererminant
gaussElim1   (xs:xss)        =   xs : gaussElim (map tail yss)
		 where
		 yss    = gaussElim_aux xs xss
gaussElim1      xs           = xs



--- rowReduce is gaussinn elimination with the augmented matrix at the back
-- neex to add pivoting to remove the devison by 0
rowReduce_aux :: [Double] -> Matrix -> [Double] -> ( [[Double]], [Double])
rowReduce_aux     (x:xs)   ((y:ys):yss)  (b:(b1:bs))  =
                          let (vss, vs) = rowReduce_aux (x:xs) yss (b: bs)  in
                                   (yms : vss, bi : vs)
				where
				    m   =  y / x
				    ms  = map (*m) (x:xs)
				    yms =  (y:ys) <-> ms
				    bi  =  b1 - m*b
rowReduce_aux      _           _            xs       = ([], tail xs)
--------------------------------------------------------------------------------------------------

rowReduce   (xs:xss)    (b:bs)   =  (xs : wss, b:ws)
		 where
		    (yss, zs)    = rowReduce_aux xs xss (b:bs)
		    (wss, ws)    = rowReduce (map tail yss)  zs
rowReduce    xs         ys       =  (xs, ys)
-----
						
--- back substitution
backSubs_aux :: Matrix -> [Double] -> [Double] -> [Double]
backSubs_aux  ((y:ys):yss)  xs (b:bs)  =  (backSubs_aux yss (k:xs)  bs)
                            where    k = (b - (xs <.> ys)) / y
backSubs_aux   _            xs   _     =  xs

backSubs           xs      ys          =   backSubs_aux  (reverse xs) [] (reverse ys)
--
-- finally we combine back Substitution and row Reduction to solve Ax = b -----------------------------
axEqB   xss  bs          =   backSubs ass zss
        where (ass, zss) = rowReduce xss bs


-- says if a matrix is a lower triangle in a matrix
isLowerTriangle :: Matrix -> Bool
isLowerTriangle      xs         =  foldr fF True zeros
                where  zxs      = zip  xs [0..]
                       pP (a,b) = fst (splitAt b a)
                       zeros    = concatMap pP zxs
                       fF a b   = b && (a == 0)
--------------------------------------------------------------------------------------------------------
------ Miscellaneous
-------------------------------------------------------------------------------------------------------


singleton :: [a] -> Bool
singleton   (x:xs)   = null xs --True
singleton    _       = False

--
isVec :: [[a]] -> Bool
isVec     []     =  False
isVec     xs     =  foldr (\b a -> not (null b) &&  singleton b && a) True  xs
