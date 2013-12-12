module MyTime (basicTime,

            ) where

import Text.Printf
import Control.Exception
import System.CPUTime
import Control.Parallel.Strategies
import Control.Monad
import System.Environment
import Data.List

basicTime :: IO t -> IO t
basicTime a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main = do
    putStrLn "Starting..."
    let list = [1..100000] ++ [555,100000000,9233,-1,45]++[-11,2923842394,9]
    let function =  sort (list ++ list ++ list ++ list)
    let function1 = nMins 10 list []
    basicTime $ function `seq` print (take 10  function) 
    putStrLn "------------------------"
    basicTime $ function1 `seq` print function1 
    putStrLn "Done."

nMins :: (Ord a , Num a) => Int -> [a] -> [a] ->  [a]
nMins  n  xs  ys | n == 0 = ys
                 | otherwise =  let y = (minimum xs)
                                    (_,rest) = partition (== y) xs
                                in    nMins (n-1) rest (y:ys)

