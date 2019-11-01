{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Main where

import FTtop
import Data.Monoid
import qualified Data.Set as S
import System.Environment
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Foldable
import Control.DeepSeq

type Pair = (Int,Int)
type FT = FingerTree (S.Set Pair) Pair 
data Setx = Setx { getSet  :: S.Set Pair, getFT  :: FT} deriving Show
data Setf = Setf { getSetF :: S.Set Pair, getFTF :: Forest} deriving Show 

type Forest = FingerTree (S.Set Pair) Setx

emptyForest :: Forest
emptyForest =  empty

instance Edges (S.Set Pair) Pair where
    edges (x,y) = S.insert (x,y) S.empty 

instance Edges (S.Set Pair) Setx where
    edges (Setx set _) = set

t1',t2',t3',t4' :: FT
t1' = fromList [(3,3),(1,1),(5,5)] 
t2' = fromList [(6,6),(2,2),(4,4)]
t3' = fromList [(7,7),(9,9),(11,11)]
t4' = fromList [(12,12),(8,8),(10,10)]
t5' = fromList [(15,15),(17,17),(13,13),(19,19)]
t6' = fromList [(14,14),(16,16),(20,20),(18,18)] 

s1' = S.fromList [(3,3),(1,1),(5,5)]
s2' = S.fromList [(6,6),(2,2),(4,4)]
s3' = S.fromList [(7,7),(9,9),(11,11)]
s4' = S.fromList [(12,12),(8,8),(10,10)]
s5' = S.fromList [(15,15),(17,17),(13,13),(19,19)]
s6' = S.fromList [(14,14),(16,16),(20,20),(18,18)] 

t1,t2,t3,t4 :: Setx
t1  = Setx s1' t1' 
t2  = Setx s2' t2' 
t3  = Setx s3' t3' 
t4  = Setx s4' t4'
t5  = Setx s5' t5'
t6  = Setx s6' t6'

f1 :: Forest
f1 =  foldr (<|) emptyForest [t3,t4,tx,t1,t2] 

tx = Setx {getSet = S.fromList [(13,13),(14,14),(15,15),(16,16),(17,17),(19,19),(18,18),(20,20)]
          , getFT = Deep (S.fromList [])
                    (Three (14,14) (16,16) (20,20))
                    (Deep (S.fromList [])
                      (One (Node2 (S.fromList [(15,15),(22,22)]) (22,22) (15,15)))
                      Empty
                      (One (Node2 (S.fromList [(13,13),(17,17)]) (17,17) (13,13))))
                    (One (19,19))}


nsetxs :: Int -> [Setx]
nsetxs 0 = []
nsetxs n = (Setx set ft) : nsetxs (n-1)
 where
    to    = n*50
    from  = (n-1)*50 
    lista = zip[from,from+1..(to-1)][from,from+1..]
    set   = S.fromList lista
    ft    = fromList lista

forestN n = foldr (<|) emptyForest (nsetxs n)

f10M = Setf (S.fromList (listZip 10000000)) (forestN 200000) 

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
    
    let ft     = fromListPairs setxEmpty (listZip n)
    let ft'    = deepseq ft ("\tFT done")
    putStr ft'
    endFT   <- getCurrentTime

    startSea1 <- getCurrentTime
    let pairN2  = (div n 2, div n 2)
--    let pred1 b = (S.member pairN2) b
    let spot1   = lookForPair pairN2 ft
    putStr $ "\t" ++ show spot1
    endSea1   <- getCurrentTime

    startSea2 <- getCurrentTime
    let p75     = div n 4
    let pairN75 = (n - p75, n - p75)
--    let pred2 b = (S.member pairN75) b
    let spot2   = lookForPair pairN75 ft
    putStr $ "\t" ++ show spot2
    endSea2   <- getCurrentTime

    startSea3 <- getCurrentTime
    let pairFalse = (n + 1, n + 1)
--    let pred3 b   = (S.member pairFalse) b
    let spot3   = lookForPair pairFalse ft
    putStr $ "\t" ++ show spot3
    endSea3   <- getCurrentTime

    startSea4 <- getCurrentTime
    let pair25  = (div n 4, div n 4)
--    let pred4 b = (S.member pair25) b
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


lookForPair pair ftx
   = case (sea pair ftx) of
     ResultSearch _ _ _ -> True
     _                  -> False
--     where
--       p b = (S.member pair) b
--       ft  = getFT ftx


instance NFData Setx where
   rnf (Setx s f) = rnf s `seq` rnf f

setxEmpty :: Setx 
setxEmpty =  Setx {getSet = S.empty,getFT = empty} 

fromListPairs :: Setx -> [Pair] -> Setx 
fromListPairs ft []     = ft
fromListPairs ft xs     =
  let set = getSet ft
      ftx = getFT  ft
      f   = foldr (<|) ftx xs
      s   = S.fromList xs 
  in  Setx {getSet = s, getFT = f}

ins :: Pair -> Setx -> Setx 
ins x ft =
  let set = getSet ft
      ftx = getFT  ft
      f   = x <| ftx
      s   = S.insert x set 
  in  Setx {getSet = s, getFT = f}

mer :: Setx -> Setx -> Setx 
mer left right = 
  let set = (getSet left) `S.union` (getSet right)
      ftx = (getFT  left)   ><    (getFT  right)
  in  set `seq` ftx `seq` Setx { getSet = set, getFT = ftx }

data ResultSearch 
   = NoResult
   | ResultSearch Setx Pair Setx 
   deriving Show

sea ::  Pair -> Setx -> ResultSearch 
sea pair setx =
 if p set then sea' p setx
 else NoResult 
 where ft  = getFT  setx
       set = getSet setx
       p b = (S.member pair) b

sea'   :: (S.Set Pair -> Bool) -> Setx -> ResultSearch 
sea' p setx =
   let ft  = getFT  setx
       set = getSet setx 
   in  case (search p ft ) of
      NoBuilt                  -> NoResult
      Built spot ltree lset x rtree rset ->
         case spot of
            LeftEnd   -> ResultSearch llset x lrset
            RightEnd  -> ResultSearch rlset x rrset
            CentreEnd -> ResultSearch lsetx x rsetx
         where
            lsetx = Setx {getSet=lset         ,getFT=ltree}
            rsetx = Setx {getSet=rset         ,getFT=rtree}
            
            llset = Setx {getSet=lset          ,getFT=ltree}
            lrset = Setx {getSet=set S.\\ xlset,getFT=rtree}

            rlset = Setx {getSet=set S.\\ xrset,getFT=ltree}
            rrset = Setx {getSet=rset          ,getFT=rtree}

            xlset = S.insert x lset -- (getPair x) lset
            xrset = S.insert x rset -- (getPair x) rset
