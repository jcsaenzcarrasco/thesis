{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, NumericUnderscores #-}

module RndDynTs where

import DataFingerTree
import DTFull
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

sndValue1stElemLastTree :: Integral b => ForestMSet b -> b -- forcing a forest to be traversed
sndValue1stElemLastTree (ForestMSet _ _ ft') = snd pair
  where
     (_     :> ft) = viewr ft' -- by extracting the last tree
     (pair' :< _ ) = viewl ft  -- and its first element, i.e.
     MSet pair = pair'        -- first element of last tree in forest

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
  case (search p tree) of
    Position _ _ _ -> (tree,False)
    _              -> (Elem x <| tree,True)
  where
    p b _ = (S.member x) b

rndSeq :: (Num a, Ord a, Random a) => (a,a) -> TreeSize -> [a]
rndSeq range rndseed = toListFTS $ initFTSRnd range rndseed

unitForestListMSet :: Integral a => [a] -> ForestMSet a
unitForestListMSet xs = unitForestListMSet' xs emptyForestMSet

unitForestListMSet' :: Integral a => [a] -> ForestMSet a -> ForestMSet a
unitForestListMSet' []     forest = forest
unitForestListMSet' nodes (ForestMSet _ _ ft) =
  let forest = foldr (<|)  ft (map (\x->Single (MSet(x,x))) nodes)
      nnodes = length nodes
  in  ForestMSet nnodes nnodes forest

lstForestMSet :: [Int] -> ForestMSet Int
lstForestMSet nodes@(x:xs) =
  let to     = length nodes
      idxs   = randomRs (0,(to-1)) (mkStdGen x)
      forest = unitForestListMSet nodes
  in  bldForestListMSet nodes idxs forest

bldForestListMSet :: Integral a => [a] -> [Int] -> ForestMSet a -> ForestMSet a
bldForestListMSet _ [] forest  = forest
bldForestListMSet _ [x] forest = forest
bldForestListMSet nodes (i:j:is) forest@(ForestMSet nnodes forestsize ft)
   | forestsize >= (2*nnodes - 1) = forest
   | otherwise  =  bldForestListMSet nodes is $ linkMSet (nodes !! i) (nodes !! j) forest

bld :: Integral a => [(a,a)] -> ForestMSet a -> ForestMSet a
bld [] forest = forest
bld [x] forest = forest
bld nodes@(i:is) forest@(ForestMSet nnodes forestsize ft)
  | forestsize >= (2*nnodes - 1) = forest
  | otherwise  =  bld is $ linkMSet (fst i)(snd i) forest

getLeftTreeMSet :: Integral a => ForestMSet a -> TreeMSet a
getLeftTreeMSet forest = tree
  where
    ForestMSet _ _ ft = forest
    tree :< _       = viewl ft

getNthTreeTop :: Integral a => Int -> ForestMSet a -> TreeMSet a
getNthTreeTop nth forest@(ForestMSet _ _ Empty)  = emptyTreeMSet
getNthTreeTop nth forest@(ForestMSet nnodes forsize ft)
   | nth <= 1   = tree
   | otherwise  = getNthTreeTop (nth - 1) (ForestMSet nnodes forsize ft')
  where
    tree :< ft' = viewl ft


rndForestMSet :: (Int,Int) -> TreeSize -> RndSeed -> ForestMSet Int
rndForestMSet (from,to) treesize rndseed =
  let from'      = min from to
      to'        = max from to
      nodes      = rndSeq (from',to') rndseed
      lnodes     = ranges treesize nodes
      ltrees     = fmap (getLeftTreeMSet . lstForestMSet) lnodes
      forest     = foldr (<|) empty ltrees
      nnodes     = (to' - from') + 1
      forestsize = nnodes + (sum ( map (S.size . getEdges . measure) ltrees ) )
  in  ForestMSet nnodes forestsize forest

rndForestMSetSeq :: Int -> [[Int]] -> ForestMSet Int
rndForestMSetSeq nnodes lnodes =
  let
      ltrees     = fmap (getLeftTreeMSet . lstForestMSet) lnodes
      forest     = foldr (<|) empty ltrees
      forestsize = nnodes + (sum ( map (S.size . getEdges . measure) ltrees ) )
  in  ForestMSet nnodes forestsize forest

rndForestTreeSizeMSet :: (Int,Int) -> TreeSize -> RndSeed -> ForestMSet Int
rndForestTreeSizeMSet range treesize rndseed
 = rndForestMSet range treesize rndseed

rndForestNTreesMSet :: (Int,Int) -> Int -> RndSeed -> ForestMSet Int
rndForestNTreesMSet (from,to) ntrees rndseed
 | ntrees < 1 = rndForestMSet (from',to') 0 rndseed
 | otherwise  = rndForestMSet (from',to') treesize rndseed
 where
   from'    = min from to
   to'      = max from to
   nnodes   = (to' - from') + 1
   nume     = div nnodes ntrees
   rema     = mod nnodes ntrees
   treesize = if (rema > 0) then (nume + 1) else nume

listDAG n = [(x,y)|x<-[1..n],y<-[x..n],x/=y]
{-
conns :: (Int,Int) -> ForestMSet Int -> Int -> ([(Int,Int)],[(Int,Int)])
conns (from,to) forest rndseed =
   let from' = min from to
       to'   = max from to
       nodes = listDAG to' -- randomRs (from',to') (mkStdGen rndseed)
   in  conns' nodes forest S.empty S.empty
conns' :: [(Int,Int)] -> ForestMSet Int -> S.Set (Int,Int) -> S.Set (Int,Int) -> ([(Int,Int)],[(Int,Int)])
conns' [] _ setTrues setFalses = (S.toList setTrues, S.toList setFalses)
conns' ((i,j):is) forest@(ForestMSet nnodes _ _) setTrues setFalses
--   | i == j    = conns' is forest setTrues setFalses
   | S.size setTrues + S.size setFalses >= div (nnodes^2 - nnodes) 2 = (S.toList setTrues, S.toList setFalses)
   | otherwise = if   fst (connectedMSet i j forest)
                 then conns' is forest setTrues' setFalses
                 else conns' is forest setTrues  setFalses'
  where
    pair       = (min i j, max i j)
    setTrues'  = S.insert pair setTrues
    setFalses' = S.insert pair setFalses
-}
conns :: (Int,Int) -> ForestMSet Int -> Int -> ([(Int,Int)],[(Int,Int)])
conns (from,to) forest rndseed =
   let from' = min from to
       to'   = max from to
--       nodes = listDAG to' -- randomRs (from',to') (mkStdGen rndseed)
       nodes = randomRs (from',to') (mkStdGen rndseed)
   in  conns' nodes forest 0 [] 0 []
--conns' :: [(Int,Int)] -> ForestMSet Int -> Int -> [(Int,Int)] -> Int -> [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
conns' :: [Int] -> ForestMSet Int -> Int -> [(Int,Int)] -> Int -> [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
conns' [] _ _ setTrues _ setFalses = (setTrues, setFalses)
--conns' ((i,j):is) forest@(ForestMSet nnodes _ _) n setTrues m setFalses
conns' (i:j:is) forest@(ForestMSet nnodes _ _) n setTrues m setFalses
   | i == j    =  conns' is forest n setTrues m setFalses
--   | n + m >= div (nnodes^2 - nnodes) 2 = (setTrues, setFalses)
   | n>=1000 && m>=1000 = (setTrues, setFalses)
   | otherwise =  if (connBoolMSet i j forest)
                  then if n>=1000
                       then conns' is forest n     setTrues  m    setFalses  -- new
                       else conns' is forest (n+1) setTrues' m    setFalses
                  else if m>=1000
                       then conns' is forest  n    setTrues  m    setFalses  -- new
                       else conns' is forest  n    setTrues (m+1) setFalses'
  where
    pair       = (min i j, max i j)
    setTrues'  = pair : setTrues
    setFalses' = pair : setFalses 

links (from,to) forest rndseed =
  let from' = min from to
      to'   = max from to
      nodes = randomRs (from',to') (mkStdGen rndseed)
  in  links' nodes forest

links' (i:j:is) forest@(ForestMSet nnodes forsize _) -- (i:j:is) is meant to be infinite
 | forsize >= (2*nnodes - 1) = []
 | otherwise                 =
    let forest'@(ForestMSet _ forsize' _) = linkMSet i j forest
    in  if forsize' > forsize
        then (i,j) : links' is forest'
        else         links' is forest

cuts (from,to) forest rndseed =
  let from' = min from to
      to'   = max from to
      nodes = randomRs (from',to') (mkStdGen rndseed)
  in  cuts' nodes forest

cuts' (i:j:is) forest@(ForestMSet nnodes forsize _)
  | forsize == nnodes = []
  | otherwise         =
     let forest'@(ForestMSet _ forsize' _) = cutMSet i j forest
     in  if forsize' < forsize
         then (i,j) : cuts' is forest'
         else         cuts' is forest

-- W A R N I N G : The result of linkcuts is an infinite list of nodes
linkcuts :: (Integral a, Random a) => (a, a) -> ForestMSet a -> Int -> ([Char], [(a, a)])
linkcuts (from,to) forest rndseed =
   let from' = min from to
       to'   = max from to
       nodes = randomRs (from',to') (mkStdGen rndseed)
   in  linkcutsA nodes forest

linkcutsA :: Integral a => [a] -> ForestMSet a -> ([Char], [(a, a)])
linkcutsA  ops@(i:j:is) forest@(ForestMSet nnodes forsize _)
 | forsize >= (2*nnodes - 1) = ("cut" ,cutlinks' ops forest)
 | otherwise                 = ("link",linkcuts' ops forest)

linkcuts' :: Integral a => [a] -> ForestMSet a -> [(a, a)]
linkcuts' (i:j:is) forest@(ForestMSet nnodes forsize _) =
--  | forsize >= (3*nnodes - 2) = cutlinks' ops forest
--  | otherwise                 =
    let forest'@(ForestMSet _ forsize' _) = linkMSet i j forest
    in  if forsize' > forsize
        then (i,j) : cutlinks' is forest'
        else         linkcuts' is forest

cutlinks' :: Integral a => [a] -> ForestMSet a -> [(a, a)]
cutlinks' (i:j:is) forest@(ForestMSet nnodes forsize _) =
 --  | forsize == nnodes         = linkcuts' ops forest
 --  | otherwise                 =
      let forest'@(ForestMSet _ forsize' _) = cutMSet i j forest
      in  if forsize' < forsize
          then (i,j) : linkcuts' is forest'
          else         cutlinks' is forest

lcList :: (Integral b, Random b) => (b, b) -> ForestMSet b -> Int -> [ForestMSet b -> ForestMSet b]
lcList range forest rndseed =
   let (fstop,ops) = linkcuts range forest rndseed
   in  if fstop=="cut" then fstcut ops else fstlink ops

appLinkCuts :: Integral a => String -> [(a,a)] -> [ForestMSet a -> ForestMSet a]
appLinkCuts fstop ops
   | fstop == "cut"   = fstcut  ops
   | fstop == "link"  = fstlink ops
   | otherwise        = error "wrong operation (should be either \"cutMSet\" or \"linkMSet\")"

fstlink :: Integral b => [(b, b)] -> [ForestMSet b -> ForestMSet b]
fstlink []       = []
fstlink [x]      = linkMSet (fst x) (snd x) : []
fstlink (i:j:is) = linkMSet (fst i) (snd i) : cutMSet (fst j) (snd j) : fstlink is


fstcut :: Integral b => [(b, b)] -> [ForestMSet b -> ForestMSet b]
fstcut []        = []
fstcut [x]       = cutMSet (fst x) (snd x) : []
fstcut (i:j:is)  = cutMSet (fst i) (snd i) : linkMSet (fst j) (snd j) : fstcut is




linkcutsSizes :: (Integral a, Random a) => (Int,Int) -> (a, a) -> ForestMSet a -> Int -> ([Char], [(a, a)])
linkcutsSizes (maxi,mini) (from,to) forest rndseed =
   let from' = min from to
       to'   = max from to
       nodes = randomRs (from',to') (mkStdGen rndseed)
   in  linkcutsAux (maxi,mini) nodes forest

linkcutsAux :: Integral a => (Int,Int) -> [a] -> ForestMSet a -> ([Char], [(a, a)])
linkcutsAux (maxi,mini) ops@(i:j:is) forest@(ForestMSet nnodes forsize _)
 | forsize >= (2*nnodes - 1) = ("cut" ,cutlinksMin' (maxi,mini) ops forest)
 | otherwise                 = ("link",linkcutsMax' (maxi,mini) ops forest)

linkcutsMax' :: Integral a => (Int,Int) -> [a] -> ForestMSet a -> [(a, a)]
linkcutsMax' (maxi,mini) (i:j:is) forest@(ForestMSet nnodes forsize _) =
--  | forsize >= (3*nnodes - 2) = cutlinks' ops forest
--  | otherwise                 =
    let forest'@(ForestMSet _ forsize' _) = linkMax maxi i j forest
    in  if forsize' > forsize
        then (i,j) : cutlinksMin' (maxi,mini) is forest'
        else         linkcutsMax' (maxi,mini) is forest

cutlinksMin' :: Integral a => (Int,Int) -> [a] -> ForestMSet a -> [(a, a)]
cutlinksMin' (maxi,mini) (i:j:is) forest@(ForestMSet nnodes forsize _) =
 --  | forsize == nnodes         = linkcuts' ops forest
 --  | otherwise                 =
      let forest'@(ForestMSet _ forsize' _) = cutMin mini i j forest
      in  if forsize' < forsize
          then (i,j) : linkcutsMax' (maxi,mini) is forest'
          else         cutlinksMin' (maxi,mini) is forest

lcListSizes :: (Integral b, Random b) => (Int,Int) -> (b, b) -> ForestMSet b -> Int -> [ForestMSet b -> ForestMSet b]
lcListSizes (maxi,mini) range forest rndseed =
   let (fstop,ops) = linkcutsSizes (maxi,mini) range forest rndseed
   in  if fstop=="cut" then fstcutS (maxi,mini) ops else fstlinkS (maxi,mini) ops

appLinkCutsSizes :: Integral a => (Int,Int) -> String -> [(a,a)] -> [ForestMSet a -> ForestMSet a]
appLinkCutsSizes (maxi,mini) fstop ops
  | fstop == "cut"   = fstcutS  (maxi,mini) ops
  | fstop == "link"  = fstlinkS (maxi,mini) ops
  | otherwise        = error "wrong operation (should be either \"cutMin\" or \"linkMax\")"

fstlinkS :: Integral b => (Int,Int) -> [(b, b)] -> [ForestMSet b -> ForestMSet b]
fstlinkS _ []                 = []
fstlinkS (maxi,_) [x]         = linkMax maxi (fst x) (snd x) : []
fstlinkS (maxi,mini) (i:j:is) = linkMax maxi (fst i) (snd i) : cutMin mini (fst j) (snd j) : fstlinkS (maxi,mini) is

fstcutS :: Integral b => (Int,Int) -> [(b, b)] -> [ForestMSet b -> ForestMSet b]
fstcutS _ []                 = []
fstcutS (_,mini) [x]         = cutMin mini (fst x) (snd x) : []
fstcutS (maxi,mini) (i:j:is) = cutMin mini (fst i) (snd i) : linkMax maxi (fst j) (snd j) : fstcutS (maxi,mini) is
