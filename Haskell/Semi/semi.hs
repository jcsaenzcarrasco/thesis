{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Main where

import FTsemi
import Data.Monoid
import qualified Data.Set as S
import System.Environment
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Foldable
import Control.DeepSeq

type Pair = (Int,Int)
type FT = FingerTree (S.Set Pair) Pair 

type Forest = FingerTree (S.Set Pair) FT

emptyForest :: Forest
emptyForest =  empty

t1,t2,t3,t4 :: FT
t1 = fromList [(3,3),(1,1),(5,5)]
t2 = fromList [(6,6),(2,2),(4,4)]
t3 = fromList [(7,7),(9,9),(11,11)]
t4 = fromList [(12,12),(8,8),(10,10)]

f1 :: Forest
f1 = (t3 <| t4 <| emptyForest ) |> t1 |> t2 

instance Edges (S.Set Pair) Pair where
    edges (x,y) = S.insert (x,y) S.empty 

listZip :: Int -> [Pair]
listZip n = zip[1..n][1..]

ft :: Int -> FT
ft n = fromList (listZip n) 

main = do
    [nx]  <- getArgs
    let n =  read nx :: Int

    startList <- getCurrentTime
    let listPairs = listZip n
    let list' = deepseq listPairs ("list done")
    putStr list'
    endList   <- getCurrentTime 

    startFT <- getCurrentTime
    let ft     = fromList (listZip n)
    let ft'    = deepseq ft ("\tFT done")
    putStr ft'
    endFT   <- getCurrentTime

    startSea1 <- getCurrentTime
    let pairN2  = (div n 2, div n 2)
    let pred1 b = (S.member pairN2) b
    let spot1   = lookForPair pairN2 ft
    putStr $ "\t" ++ show spot1
    endSea1   <- getCurrentTime

    startSea2 <- getCurrentTime
    let p75     = div n 4
    let pairN75 = (n - p75, n - p75)
    let pred2 b = (S.member pairN75) b
    let spot2   = lookForPair pairN75 ft
    putStr $ "\t" ++ show spot2
    endSea2   <- getCurrentTime

    startSea3 <- getCurrentTime
    let pairFalse = (n + 1, n + 1)
    let pred3 b   = (S.member pairFalse) b
    let spot3   = lookForPair pairFalse ft
    putStr $ "\t" ++ show spot3
    endSea3   <- getCurrentTime

    startSea4 <- getCurrentTime
    let pair25  = (div n 4, div n 4)
    let pred4 b = (S.member pair25) b
    let spot4   = lookForPair pair25 ft
    putStr $ "\t" ++ show spot4
    endSea4   <- getCurrentTime

    let tsea1 = endSea1 `diffUTCTime` startSea1 
    let tsea2 = endSea2 `diffUTCTime` startSea2
    let tsea3 = endSea3 `diffUTCTime` startSea3
    let tsea4 = endSea4 `diffUTCTime` startSea4


    putStr $ "\n Time list creation: " ++ show (endList `diffUTCTime` startList) ++
             "\n Time FT creation  : " ++ show (endFT `diffUTCTime` startFT) 
    putStrLn $ "\nSEARCH(N/2) : " ++ show tsea1 ++
               "\nSEARCH(N3/4): " ++ show tsea2 ++
               "\nSEARCH(N+1) : " ++ show tsea3 ++
               "\nSEARCH(N/4) : " ++ show tsea4  ++ "\n"  


lookForPair pair ft
   = case (search p ft) of
     Built _ _ _ -> True
     _           -> False
     where
       p b = (S.member pair) b
