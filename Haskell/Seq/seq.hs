
module Main where

import qualified FTseq as FT
import System.Environment
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Foldable
import Control.DeepSeq 
import Data.Tree

type Pair = (Int,Int)
type FT = FT.Seq Pair


tree1 = Node 1 [] 

forest :: FT.Seq (Tree Int)
forest = tree1 FT.<| FT.empty


listZip :: Int -> [Pair]
listZip n = zip[1..n][1..]

ft :: Int -> FT
ft n = FT.fromList (listZip n) 

main = do
    [nx]  <- getArgs
    let n =  read nx :: Int

    startList <- getCurrentTime
    let listPairs = listZip n
    let list' = deepseq listPairs ("list done")
    putStr list'
    endList   <- getCurrentTime 

    startFT <- getCurrentTime
    let ft     = FT.fromList (listZip n)
    let ft'    = deepseq ft ("\tFT done")
    putStr ft'
    endFT   <- getCurrentTime

    startSea1 <- getCurrentTime
    let pairN2 = (div n 2, div n 2)
    
    let spot1  = FT.elemIndexL pairN2 ft
    putStr $ "\t" ++ show spot1
    endSea1   <- getCurrentTime

    startSea2 <- getCurrentTime
    let p75     = div n 4
    let pairN75 = (n - p75, n - p75)
    
    let spot2   = FT.elemIndexL pairN75 ft
    putStr $ "\t" ++ show spot2
    endSea2   <- getCurrentTime

    startSea3 <- getCurrentTime
    let pairFalse = (n + 1, n + 1)
    
    let spot3     = FT.elemIndexL pairFalse ft
    putStr $ "\t" ++ show spot3
    endSea3   <- getCurrentTime

    startSea4 <- getCurrentTime
    let pair25 = (div n 4, div n 4)
    
    let spot4     = FT.elemIndexL pair25 ft
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
 
--    putStrLn "\n\nEverything is FINE\n"