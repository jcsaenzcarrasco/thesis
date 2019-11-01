{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Main where

import FTfull
import Data.Monoid
import qualified Data.Set as S
import System.Environment
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Control.DeepSeq

type Pair = (Int,Int)
type FT = FingerTree SetPair Pair
type SetPair = S.Set Pair
type Forest = FingerTree (S.Set Pair) FT

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
     Position _ _ _ -> True
     _              -> False
     where
       p b a = (S.member pair) b

pr1,sf1 :: Digit Pair
pr2,sf2 :: Digit (Node SetPair Pair)
single  :: FingerTree SetPair (Node SetPair (Node SetPair Pair))
pr1 = Four (2,2) (3,3) (4,4) (5,5)
sf1 = Four (42,42) (43,43) (44,44) (45,45)
pr2 = Four (node3 (6,6) (7,7) (8,8)) (node3 (9,9)(10,10)(11,11)) (node3 (12,12)(13,13)(14,14)) (node3 (15,15)(16,16)(17,17))
sf2 = Four (node3 (30,30) (31,31) (32,32)) (node3 (33,33)(34,34)(35,35)) (node3 (36,36)(37,37)(38,38)) (node3 (39,39)(40,40)(41,41))
single = Single (node3 (node3 (18,18)(19,19)(20,20))
                       (node3 (21,21)(22,22)(23,23))
                       (node3 (24,24)(25,25)(26,26)))

mi' :: FingerTree SetPair (Node SetPair Pair)
mi'  = Deep (edges pr2 <> edges single <> edges sf2) pr2 single sf2
ft' :: FT
ft' = Deep (edges pr1 <> edges mi' <> edges sf1) pr1 mi' sf1

mi2 = Deep (edges pr2 <> edges sf2) pr2 Empty sf2
ft2 = Deep (edges pr1 <> edges mi2 <> edges sf1) pr1 mi2 sf1

ft3 = Deep (edges pr1 <> edges sf1) pr1 Empty sf1
