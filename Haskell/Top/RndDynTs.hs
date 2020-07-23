{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, NumericUnderscores #-}

module RndDynTs where

import DataFingerTreeTOP
import qualified DataFingerTree as FT
import DynTreesTop
import EtRt
import Data.Set (Set,member,fromList)
import qualified Data.Set as S
import Data.Monoid
import System.Random
import Data.Foldable (foldl')
import Data.List (sort)

type RndSeed  = Int
type TreeSize = Int

ranges :: Ord a => TreeSize -> [a] -> [[a]]
ranges _  [] = []
ranges n sek
  | n < 1     = [sek]
  | otherwise = (take n sek) : ranges n (drop n sek)

sndValue1stElemLastTree :: Integral b => ForestTop b -> b -- forcing a forest to be traversed
sndValue1stElemLastTree (ForestTop _ ft') = snd pair
  where
     (_     :> ft) = viewr ft'          -- by extracting the last tree
     (pair' :< _ ) = viewl (ftTree ft)  -- and its first element, i.e.
     Leaf pair = pair'                  -- first element of last tree in forest

data Elem a = Elem a
instance Show a => Show (Elem a) where
   show (Elem a) = show a
instance Ord a => FT.Measured (S.Set a) (Elem a) where
   measure (Elem x) = S.insert x S.empty

type FTS a = FT.FingerTree (S.Set a) (Elem a)

emptyFTS :: Ord a => FTS a
emptyFTS =  FT.empty

toListFTS :: Ord a => FTS a -> [a]
toListFTS ft = case (FT.viewl ft) of
   FT.EmptyL    -> []
   Elem x FT.:< rest -> x : toListFTS rest

initFTSRnd :: (Num a, Ord a, Random a)
            => (a,a) -> RndSeed -> FTS a
initFTSRnd range rndseed = initFTSRnd' range rndseed emptyFTS

initFTSRnd' :: (Num a, Ord a, Random a)
            => (a,a) -> RndSeed -> FTS a -> FTS a
initFTSRnd' (from,to) rndseed tree = initFTS' n listValues tree
 where
  n       = abs(to - from) + 1
  listValues = randomRs (from,to) (mkStdGen rndseed)

initFTS' :: (Num a, Ord a) => a -> [a] -> FTS a -> FTS a
initFTS' _ []     tree = tree
initFTS' 0 _      tree = tree
initFTS' n (x:xs) tree =
  case (insFTSNode x tree) of
    (ft,True)  -> initFTS' (n-1) xs ft
    (ft,False) -> initFTS' n     xs ft

insFTSNode :: Ord a => a -> FTS a -> (FTS a, Bool)
insFTSNode x tree =
  case (FT.search p tree) of
    FT.Position _ _ _ -> (tree,False)
    _                 -> (Elem x FT.<| tree,True)
  where
    p b _ = (S.member x) b

rndSeq :: (Num a, Ord a, Random a) => (a,a) -> RndSeed -> [a]
rndSeq range rndseed = toListFTS $ initFTSRnd range rndseed

unitForestListTop :: Integral a => [a] -> ForestTop a
unitForestListTop xs = unitForestListTop' xs emptyForestTop

unitForestListTop' :: Integral a => [a] -> ForestTop a -> ForestTop a
unitForestListTop' []     forest = forest
unitForestListTop' nodes (ForestTop _ ft) =
  let forest = foldr (<|) ft (map (\x->TreeTop (buildMSet (x,x) emptyMSet) (Single (Leaf(x,x)))) nodes)
      mset   = foldr buildMSet emptyMSet (map (\x->(x,x)) nodes)
  in  ForestTop mset forest

lstForestTop :: [Int] -> ForestTop Int
lstForestTop nodes@(x:xs) =
  let to     = length nodes
      idxs   = randomRs (0,(to-1)) (mkStdGen x)
      forest = unitForestListTop nodes
  in  bldForestListTop nodes idxs forest

bldForestListTop :: Integral a => [a] -> [Int] -> ForestTop a -> ForestTop a
bldForestListTop _ []  forest = forest
bldForestListTop _ [x] forest = forest
bldForestListTop nodes (i:j:is) forest
   | sizeForest forest >= (3*(nnodesForest forest) - 2) = forest
   | otherwise  =  bldForestListTop nodes is $ link (nodes !! i) (nodes !! j) forest

bld :: (Integral a,Random a) => [(a,a)] -> ForestTop a -> ForestTop a
bld []  forest = forest
bld [x] forest = forest
bld nodes@(i:is) forest
  | sizeForest forest >= (3*(nnodesForest forest) - 2) = forest
  | otherwise  =  bld is $ link (fst i)(snd i) forest

getLeftTreeTop :: Integral a => ForestTop a -> TreeTop a
getLeftTreeTop forest = tree
  where  tree :< _ = viewl (ftForest forest)

getNthTreeTop :: Integral a => Int -> ForestTop a -> TreeTop a
getNthTreeTop nth forest@(ForestTop mset Empty)  = emptyTreeTop
getNthTreeTop nth forest@(ForestTop mset ft)
   | nth <= 1   = tree
   | otherwise  = getNthTreeTop (nth - 1) (ForestTop mset ft')
  where
    tree :< ft' = viewl ft


rndForestTop :: (Int,Int) -> TreeSize -> RndSeed -> ForestTop Int
rndForestTop (from,to) treesize rndseed =
  let from'   = min from to
      to'     = max from to
      nodes   = rndSeq (from',to') rndseed
      lnodes  = ranges treesize nodes
      ltrees  = fmap (getLeftTreeTop . lstForestTop) lnodes
      forest  = foldr (<|) empty ltrees
      mset    = foldr mappend emptyMSet (map msetTree ltrees)
  in  ForestTop mset forest

rndForestTopSeq :: Int -> [[Int]] -> ForestTop Int
rndForestTopSeq nnodes lnodes =
  let
      ltrees  = fmap (getLeftTreeTop . lstForestTop) lnodes
      forest  = foldr (<|) empty ltrees
      mset    = foldr mappend emptyMSet (map msetTree ltrees)
  in  ForestTop mset forest


rndForestTopTreeSize :: (Int,Int) -> TreeSize -> RndSeed -> ForestTop Int
rndForestTopTreeSize range treesize rndseed
   = rndForestTop range treesize rndseed

rndForestTopNTrees :: (Int,Int) -> Int -> RndSeed -> ForestTop Int
rndForestTopNTrees (from,to) ntrees rndseed
  | ntrees < 1 = rndForestTop (from',to') 0 rndseed
  | otherwise  = rndForestTop (from',to') treesize rndseed
 where
   from'    = min from to
   to'      = max from to
   nnodes   = (to' - from') + 1
   nume     = div nnodes ntrees
   rema     = mod nnodes ntrees
   treesize = if (rema > 0) then (nume + 1) else nume

conns :: (Int,Int) -> ForestTop Int -> Int -> Int -> ([(Int,Int)],[(Int,Int)])
conns (from,to) forest nrunsf rndseed =
  let from' = min from to
      to'   = max from to
      nodes = randomRs (from',to') (mkStdGen rndseed)
  in  conns' nodes forest nrunsf 0 [] 0 []
conns' :: [Int] -> ForestTop Int -> Int -> Int -> [(Int,Int)] -> Int -> [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
conns' [] _ _ _ setTrues _ setFalses = (setTrues, setFalses)
conns' (i:j:is) forest nrunsf n setTrues m setFalses
  | i == j    =  conns' is forest nrunsf n setTrues m setFalses
  | n>=nrunsf && m>=nrunsf = (setTrues, setFalses)
  | otherwise =  if (connBool i j forest)
                 then if n>=nrunsf  -- 1000 or 500 generally
                      then conns' is forest nrunsf n     setTrues  m    setFalses  -- new
                      else conns' is forest nrunsf (n+1) setTrues' m    setFalses
                 else if m>=nrunsf  -- 1000 or 500 generally
                      then conns' is forest nrunsf n    setTrues  m    setFalses  -- new
                      else conns' is forest nrunsf n    setTrues (m+1) setFalses'
 where
--   pair       = (min i j, max i j)
   setTrues'  = (i,j) : setTrues
   setFalses' = (i,j) : setFalses



links (from,to) forest rndseed =
  let from' = min from to
      to'   = max from to
      nodes = randomRs (from',to') (mkStdGen rndseed)
  in  links' nodes forest

links' (i:j:is) forest -- (i:j:is) is meant to be infinite
 | size >= (3*(nnodesForest forest) - 2) = []
 | otherwise                 =
    let forest' = link i j forest
        size'   = sizeForest forest'
    in  if size' > size
        then (i,j) : links' is forest'
        else         links' is forest
 where size = sizeForest forest

cuts (from,to) forest rndseed =
  let from' = min from to
      to'   = max from to
      nodes = randomRs (from',to') (mkStdGen rndseed)
  in  cuts' nodes forest

cuts' (i:j:is) forest
   | size == nnodes = []
   | otherwise      =
       let forest' = cut i j forest
           size'   = sizeForest forest'
       in  if size' < size
           then (i,j) : cuts' is forest'
           else         cuts' is forest
 where
   size   = sizeForest forest
   nnodes = nnodesForest forest


-- W A R N I N G : The result of linkcuts is an infinite list of nodes
linkcuts :: (Integral a, Random a) => (a, a) -> ForestTop a -> RndSeed -> ([Char], [(a, a)])
linkcuts (from,to) forest rndseed =
   let from' = min from to
       to'   = max from to
       nodes = randomRs (from',to') (mkStdGen rndseed)
   in  linkcutsA nodes forest

linkcutsA :: Integral a => [a] -> ForestTop a -> ([Char], [(a, a)])
linkcutsA  ops@(i:j:is) forest
   | size >= (3*nnodes - 2) = ("cut" ,cutlinks' ops forest)
   | otherwise              = ("link",linkcuts' ops forest)
 where size   = sizeForest forest
       nnodes = nnodesForest forest

linkcuts' :: Integral a => [a] -> ForestTop a -> [(a, a)]
linkcuts' (i:j:is) forest =
--  | forsize >= (3*nnodes - 2) = cutlinks' ops forest
--  | otherwise                 =
    let forest' = link i j forest
        size' = sizeForest forest'
        size  = sizeForest forest
    in  if size' > size
        then (i,j) : cutlinks' is forest'
        else         linkcuts' is forest

cutlinks' :: Integral a => [a] -> ForestTop a -> [(a, a)]
cutlinks' (i:j:is) forest =
 --  | forsize == nnodes         = linkcuts' ops forest
 --  | otherwise                 =
      let forest' = cut i j forest
          size  = sizeForest forest
          size' = sizeForest forest'
      in  if size' < size
          then (i,j) : linkcuts' is forest'
          else         cutlinks' is forest

lcList :: (Integral b, Random b) => (b, b) -> ForestTop b -> RndSeed -> [ForestTop b -> ForestTop b]
lcList range forest rndseed =
   let (fstop,ops) = linkcuts range forest rndseed
   in  if fstop=="cut" then fstcut ops else fstlink ops

appLinkCuts :: Integral a => String -> [(a,a)] -> [ForestTop a -> ForestTop a]
appLinkCuts fstop ops
   | fstop == "cut"   = fstcut  ops
   | fstop == "link"  = fstlink ops
   | otherwise        = error "wrong operation (should be either \"cutMSet\" or \"linkMSet\")"

fstlink :: Integral b => [(b, b)] -> [ForestTop b -> ForestTop b]
fstlink []       = []
fstlink [x]      = link (fst x) (snd x) : []
fstlink (i:j:is) = link (fst i) (snd i) : cut (fst j) (snd j) : fstlink is


fstcut :: Integral b => [(b, b)] -> [ForestTop b -> ForestTop b]
fstcut []        = []
fstcut [x]       = cut (fst x) (snd x) : []
fstcut (i:j:is)  = cut (fst i) (snd i) : link (fst j) (snd j) : fstcut is

{-

linkcutsSizes :: (Integral a, Random a) => (Int,Int) -> (a, a) -> ForestTop a -> Int -> ([Char], [(a, a)])
linkcutsSizes (maxi,mini) (from,to) forest rndseed =
   let from' = min from to
       to'   = max from to
       nodes = randomRs (from',to') (mkStdGen rndseed)
   in  linkcutsAux (maxi,mini) nodes forest

linkcutsAux :: Integral a => (Int,Int) -> [a] -> ForestTop a -> ([Char], [(a, a)])
linkcutsAux (maxi,mini) ops@(i:j:is) forest@(ForestTop nnodes forsize _)
 | forsize >= (2*nnodes - 1) = ("cut" ,cutlinksMin' (maxi,mini) ops forest)
 | otherwise                 = ("link",linkcutsMax' (maxi,mini) ops forest)

linkcutsMax' :: Integral a => (Int,Int) -> [a] -> ForestTop a -> [(a, a)]
linkcutsMax' (maxi,mini) (i:j:is) forest@(ForestTop nnodes forsize _) =
--  | forsize >= (3*nnodes - 2) = cutlinks' ops forest
--  | otherwise                 =
    let forest'@(ForestTop _ forsize' _) = linkMax maxi i j forest
    in  if forsize' > forsize
        then (i,j) : cutlinksMin' (maxi,mini) is forest'
        else         linkcutsMax' (maxi,mini) is forest

cutlinksMin' :: Integral a => (Int,Int) -> [a] -> ForestTop a -> [(a, a)]
cutlinksMin' (maxi,mini) (i:j:is) forest@(ForestTop nnodes forsize _) =
 --  | forsize == nnodes         = linkcuts' ops forest
 --  | otherwise                 =
      let forest'@(ForestTop _ forsize' _) = cutMin mini i j forest
      in  if forsize' < forsize
          then (i,j) : linkcutsMax' (maxi,mini) is forest'
          else         cutlinksMin' (maxi,mini) is forest

lcListSizes :: (Integral b, Random b) => (Int,Int) -> (b, b) -> ForestTop b -> Int -> [ForestTop b -> ForestTop b]
lcListSizes (maxi,mini) range forest rndseed =
   let (fstop,ops) = linkcutsSizes (maxi,mini) range forest rndseed
   in  if fstop=="cut" then fstcutS (maxi,mini) ops else fstlinkS (maxi,mini) ops
-}
appLinkCutsSizes :: Integral a => (Int,Int) -> String -> [(a,a)] -> [ForestTop a -> ForestTop a]
appLinkCutsSizes (maxi,mini) fstop ops
  | fstop == "cut"   = fstcutS  (maxi,mini) ops
  | fstop == "link"  = fstlinkS (maxi,mini) ops
  | otherwise        = error "wrong operation (should be either \"cutMin\" or \"linkMax\")"

fstlinkS :: Integral b => (Int,Int) -> [(b, b)] -> [ForestTop b -> ForestTop b]
fstlinkS _ []                 = []
fstlinkS (maxi,_) [x]         = linkMax maxi (fst x) (snd x) : []
fstlinkS (maxi,mini) (i:j:is) = linkMax maxi (fst i) (snd i) : cutMin mini (fst j) (snd j) : fstlinkS (maxi,mini) is

fstcutS :: Integral b => (Int,Int) -> [(b, b)] -> [ForestTop b -> ForestTop b]
fstcutS _ []                 = []
fstcutS (_,mini) [x]         = cutMin mini (fst x) (snd x) : []
fstcutS (maxi,mini) (i:j:is) = cutMin mini (fst i) (snd i) : linkMax maxi (fst j) (snd j) : fstcutS (maxi,mini) is
