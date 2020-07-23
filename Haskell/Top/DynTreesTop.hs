{-#  LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module DynTreesTop where

import DataFingerTreeTOP
import qualified Data.Set as S
import Data.Maybe (fromJust)
import GHC.Exts (IsList(..))
import Data.Monoid ((<>))
import EtRt
import Data.Foldable (foldl')
import Data.List (sort)

midList :: Int -> [(Int,Int)] -> [(Int,Int)]
midList n l =
  let len = div (length l) 2
      l'  = drop (len -(div n 2)) l
  in  take n l'

ttop1,ttop2,ttop8,ttop9 :: TreeTop Int
ttop1   =
  let pairs  =  [(1,1),(1,2),(2,2),(2,1),(1,3),(3,3),(3,1),(1,4),(4,4),(4,1)]
      mset   =  foldr buildMSet emptyMSet pairs
      ft     =  foldl (|>) empty (map Leaf pairs)
  in  TreeTop mset ft

ttop2   =
  let pairs  =  [(7,7),(7,6),(6,6),(6,5),(5,5),(5,6),(6,7)]
      mset   =  foldl (flip(\x->buildMSet x)) emptyMSet pairs
      ft     =  foldl (|>) empty (map Leaf pairs)
  in  TreeTop mset ft

ttop8   = TreeTop (buildMSet (8,8) emptyMSet) (Single (Leaf (8,8)))
ttop9   = TreeTop (buildMSet (9,9) emptyMSet) (Single (Leaf (9,9)))

ftop1,ftop2 :: ForestTop Int
ftop1 =  ForestTop mset ((ttop1 <| empty) |> ttop2)
  where
    mset = msetTree ttop1 `mappend` msetTree ttop2

ftop2 =
  let topTreeList  =  [ttop9,ttop1,ttop8,ttop2]
      msetForest   =  foldr mappend emptyMSet (map msetTree topTreeList)
      ftTopTree    =  foldr (<|) empty topTreeList
  in  ForestTop msetForest ftTopTree

f = link 8 3 $ link 9 8 $ link 6 3 ftop2
g = cut  6 3 $ cut  3 1 f

searchMSet :: (Integral a, Measured (MultiSet a) b) =>
              (a,a) -> MultiSet a -> FingerTree (MultiSet a) b -> SearchResult (MultiSet a) b
-- example : Position (ls,l) x (rs,r) = searchMSet (3,3) (msetForest ftop2) (ftForest ftop2)
-- lforest = ForestTop ls l ;  rforest = ForestTop rs r ; joinforest = ForestTop (mappend ls rs) (l >< r)
searchMSet p@(x,y) mset ftree
    | x==y && even x = let predicate setx   = (S.member x) (getEvens setx) in search predicate mset ftree
    | x==y && odd  x = let predicate setx   = (S.member x) (getOdds  setx) in search predicate mset ftree
    | otherwise      = let predicate setx   = (S.member p) (getEdges setx) in search predicate mset ftree


searchEdge :: (Integral a, Measured (MultiSet a) b)
              => (a,a) -> MultiSet a -> FingerTree (MultiSet a) b
              -> SearchResult (MultiSet a) b
searchEdge pair mset ftree =
  let predicate setx   = (S.member pair) (getEdges setx)
--      pair = (min u v, max u v)
  in  search predicate mset ftree

-- ----------------------------------------------------------
--           ROOT of tree
rootTree :: Integral a => TreeTop a -> Maybe a
rootTree tree = case viewl (ftTree tree) of
  EmptyL      -> Nothing
  Leaf x :< _ -> Just $ fst x

-- ----------------------------------------------------------
--           REroot of tree
rerootTree :: Integral a => TreeTop a -> a -> TreeTop a
rerootTree tree@(TreeTop _ Empty) _      = tree
rerootTree tree@(TreeTop mset ft) vertex = case (searchMSet (vertex,vertex) mset ft) of
  Position (_,left) _ (_,right) -> TreeTop mset (Leaf (vertex,vertex) <| (right >< left))
  _                             -> tree

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--            CONNECTED ( u, v ) in same tree ? IN WHICH tour ?
nodeInForest :: Integral a => a -> ForestTop a -> Maybe (TreeTop a, a)
nodeInForest _      (ForestTop _ Empty) = Nothing
nodeInForest vertex (ForestTop mset ft) =
  case (searchMSet (vertex,vertex) mset ft) of
    Position _ tree _ ->
      case (rootTree tree) of
        Nothing   -> Nothing
        Just root -> Just (tree, root)
    _             -> Nothing     -- node v not in forest

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x

edgeInForest :: Integral a => (a,a) -> ForestTop a -> Maybe(TreeTop a, ForestTop a, ForestTop a)
edgeInForest _    (ForestTop _ Empty) = Nothing
edgeInForest pair (ForestTop mset ft) =
  case (searchEdge pair mset ft) of
    Position (ls,left) tree (rs,right)
                      -> Just (tree,ForestTop ls left, ForestTop rs right)
    _                 -> Nothing

-- | MAIN function regarding connectivity topic
-- | Are these two nodes connected in the provided forest?
connBool :: Integral a => a -> a -> ForestTop a -> Bool
connBool x y forest@(ForestTop mset ft) =
--  case (searchMSet (x,y) mset ft) of
--    Position _ _ _ -> True
--    _              ->
     case (nodeInForest x forest, nodeInForest y forest) of
      (Nothing          , _           ) -> False
      (_                , Nothing     ) -> False
      (Just (_,rx)     , Just (_,ry)) ->
       if rx == ry  then True else False

connected :: Integral a => a -> a -> ForestTop a
              -> (Bool, Maybe (TreeTop a, a, TreeTop a, a) )
connected x y f =
  case (nodeInForest x f, nodeInForest y f) of
    (Nothing          , _           ) -> (False, Nothing)
    (_                , Nothing     ) -> (False, Nothing)
    (Just (tx,rx)     , Just (ty,ry)) ->
     if rx == ry then (True,  Just(tx,rx,tx,rx)) else (False, Just(tx,rx,ty,ry))


-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--                           L I N K  (for trees and forest)
-- Safe? this link is for cases when it is called off the forest
linkTreeUnsafe :: -- unsafe is 'safe' when used after the forest's link verifications
   Integral a => a -> TreeTop a -> a -> TreeTop a -> TreeTop a
linkTreeUnsafe u tu v (TreeTop msetv tv) =
   let from = rerootTree tu u
       (Position (ls,left) _ (rs,right)) = searchMSet (v,v) msetv tv
--       pair = (min u v,max u v)
       ft   = ((left |> Leaf (v,v)) |> Leaf (v,u)) >< (ftTree from) >< (Leaf (u,v) <| right)
       mset = foldr buildMSet (mappend msetv (msetTree tu)) [(v,u),(u,v)]
   in  TreeTop mset ft

link :: Integral a => a -> a -> ForestTop a -> ForestTop a
link x y forest@(ForestTop mset ft) =
      case connected x y forest of
        (False, Just(tx,rx,ty,ry)) -> linkAll (linkTreeUnsafe x tx y ty)
        _                          -> forest
  where
    linkAll tree = ForestTop msetn (tree <| (lf >< rf))
    msetn  = foldr buildMSet mset [(x,y),(y,x)]
    Position (_,lf') _ (_,rf') = searchMSet (x,x) mset ft
    Position (_,lf)  _ (_,rf)  = searchMSet (y,y) mset (lf' >< rf')

linkMax :: Integral a => Int -> a -> a -> ForestTop a -> ForestTop a
linkMax maxi x y forest@(ForestTop mset ft)
  | x == y            = forest
  | otherwise         =
      case connected x y forest of
        (False, Just(tx,rx,ty,ry)) -> case (linkTreeMax maxi x tx y ty) of
                                         Just tree -> linkAll tree
                                         Nothing   -> forest
        _                          -> forest
 where
  linkAll tree = ForestTop msetn (tree <| (lf >< rf))
  msetn        = foldr buildMSet mset [(x,y),(y,x)]
  Position (_,lf') _ (_,rf') = searchMSet (x,x) mset ft
  Position (_,lf)  _ (_,rf)  = searchMSet (y,y) mset (lf' >< rf')

linkTreeMax :: Integral a => Int -> a -> TreeTop a -> a -> TreeTop a -> Maybe (TreeTop a)
linkTreeMax maxi u tu v tv'@(TreeTop msetv tv)
  | (nnodesTree tu) + (nnodesTree tv') > maxi = Nothing
  | otherwise = Just $
      let from = rerootTree tu u
          (Position (ls,left) _ (rs,right)) = searchMSet (v,v) msetv tv
--        pair = (min u v,max u v)
          ft   = ((left |> Leaf (v,v)) |> Leaf (v,u)) >< (ftTree from) >< (Leaf (u,v) <| right)
          mset = foldr buildMSet (mappend msetv (msetTree tu)) [(v,u),(u,v)]
      in  TreeTop mset ft

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--                             C U T   (for trees and forest)
--
{-
cutTreeUnsafe :: Integral a => a -> a -> TreeTop a -> (TreeTop a,TreeTop a)
cutTreeUnsafe _ _ tree@(TreeTop _ Empty) = tree
cutTreeUnsafe u v tree@(TreeTop mset ft) = case searchEdge pair mset ft of
    Position (ls,left) _ (rs,right) ->
      case (searchEdge pair ls left) of
        Position leftL _ rightL -> (rightL, leftL >< right)
        _                       ->
          case (searchEdge pair rs right) of
            Position leftR _ rightR -> (leftR,  left  >< rightR)
            _                       -> undefined -- error "Tree malformed for "++show v++" and "++show u
    _                     -> undefined -- error "Tree malformed for "++show u++" and "++show v
 where
   pair = (min u v, max u v)
-}

cutMin :: Integral a => Int -> a -> a -> ForestTop a -> ForestTop a
cutMin mini x y forest@(ForestTop mset ft) =
   case edgeInForest (x,y) forest of
     Nothing                 -> forest
     Just (tree,ltFor,rtFor) -> case (cutTreeMin mini x y tree) of
           Just (lefty, righty) -> buildForest (lefty,righty) (ftForest ltFor) (ftForest rtFor)
--           buildForest (cutUnsafeTreeMSet x y tree) ltFor rtFor
           Nothing -> forest
 where
   msetn   = foldr delPairMSet mset [(x,y),(y,x)]
   buildForest (leftTree,rightTree) lFor rFor
      = ForestTop msetn (leftTree <| rightTree <| (lFor >< rFor))

nnodesftTree msett = (S.size (getEvens msett))+(S.size (getOdds msett))

-- unsafe is 'safe' when used after the forest's CUT verifications
cutTreeUnsafe :: Integral a => a -> a -> TreeTop a -> (TreeTop a,TreeTop a)
cutTreeUnsafe u v tree@(TreeTop mset ft) = case searchEdge pair mset ft of
    Position (ls,left) _ (rs,right) ->
      case (searchEdge pair' ls left) of
        Position (lsL,leftL) _ (rsL,rightL) -> (TreeTop rsL rightL, TreeTop (mappend lsL rs)(leftL >< right))
        _                       ->
          case (searchEdge pair' rs right) of
            Position (lsR,leftR) _ (rsR,rightR) -> (TreeTop lsR leftR, TreeTop (mappend ls rsR) (left  >< rightR))
            _                       -> undefined -- error "Tree malformed for "++show v++" and "++show u
    _                     -> undefined -- error "Tree malformed for "++show u++" and "++show v
 where
   pair  = (u,v) ; pair' = (v,u)

cutTreeMin :: Integral a => Int -> a -> a -> TreeTop a -> Maybe (TreeTop a,TreeTop a)
cutTreeMin mini u v tree@(TreeTop mset ft) = case searchEdge pair mset ft of
   Position (ls,left) _ (rs,right) ->
     case (searchEdge pair' ls left) of
       Position (lsL,leftL) _ (rsL,rightL) ->
           case ((nnodesftTree rsL)>=mini, (nnodesftTree lsL + nnodesftTree rs)>= mini ) of
              (False, _)  -> Nothing
              (_, False)  -> Nothing
              (True,True) -> Just $ (TreeTop rsL rightL, TreeTop (mappend lsL rs) (leftL  >< right))
       _                       ->
         case (searchEdge pair' rs right) of
           Position (lsR,leftR) _ (rsR,rightR) ->
                 case ((nnodesftTree lsR)>=mini, (nnodesftTree ls + nnodesftTree rsR)>= mini ) of
                   (False, _)  -> Nothing
                   (_, False)  -> Nothing
                   (True,True) -> Just $ (TreeTop lsR leftR, TreeTop (mappend ls rsR) (left  >< rightR))
           _                       -> undefined -- error "Tree malformed for "++show v++" and "++show u
   _                     -> undefined -- error "Tree malformed for "++show u++" and "++show v
  where
     pair = (u,v) ;  pair' = (v,u)

cut :: Integral a => a -> a -> ForestTop a -> ForestTop a
cut x y forest@(ForestTop mset ft) =
    case edgeInForest (x,y) forest of
      Nothing                 -> forest
      Just (tree,ltFor,rtFor) -> buildForest (cutTreeUnsafe x y tree) (ftForest ltFor) (ftForest rtFor)
  where
    msetn   = foldr delPairMSet mset [(x,y),(y,x)]
    buildForest (leftTree,rightTree) lFor rFor
       = ForestTop msetn (leftTree <| ((lFor >< rFor) |> rightTree))


affixes :: ForestTop a -> ([String],String,[String])
affixes forest = affixes' (ftForest forest) ([],"",[])

affixes' :: FingerTree v a -> ([String],String,[String]) -> ([String],String,[String])
affixes' Empty      (pr,_,sf) = (pr,"Empty" , sf)
affixes' (Single _) (pr,_,sf) = (pr,"Single", sf)
affixes' (Deep _ pre' mid suf') (pr,m,sf) =
 let  pre = pr ++ [aff pre']
      suf = (aff suf') : sf
 in   affixes' mid (pre,m,suf)  -- affixes' mid (pre,m,suf)

aff (One _       ) = "I"
aff (Two _ _     ) = "II"
aff (Three _ _ _ ) = "III"
aff (Four _ _ _ _) = "IV"
