{-#  LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module DTFull where

import DataFingerTree -- ((<|),(|>),(><),search,empty,Measured,ViewL(EmptyL,(:<)),SearchResult(Position),FingerTree,viewl,measure)
import EtRt
import qualified Data.Set as S
import Data.Maybe (fromJust)
import GHC.Exts (IsList(..))
import Data.Monoid ((<>))
import Control.DeepSeq
import Data.Foldable (foldl')
import Data.List (sort)

-- ************************************************************
--    FT (Set (a,a))  (a,a)
type NumNodes   = Int -- Also edges of the form (v,v)
type ForestSize = Int -- Also is the total number of edges including NumNodes

newtype FT a    = FT {getFT :: FingerTree (S.Set (a,a)) (TreeEF a)} deriving Show

type TreeEF   a = FingerTree (S.Set (a,a)) (a,a)
data ForestEF a = ForestEF NumNodes ForestSize (FT a)

newtype MSet a     = MSet (a,a) deriving (Read,Show)
data    MultiSet a = MultiSet {getEvens :: S.Set a,
                               getOdds  :: S.Set a,
                               getEdges :: S.Set (a,a)} deriving (Read,Show)

type TreeMSet a   = FingerTree (MultiSet a) (MSet a) -- deriving Show
data ForestMSet a = ForestMSet NumNodes ForestSize (FingerTree (MultiSet a)(TreeMSet a)) deriving (Read,Show)

instance (NFData a) => NFData (MSet a) where
   rnf (MSet pair) = pair `seq` ()
--instance (NFData a) => NFData (TreeMSet a) where
--   rnf (TreeTop mset ft) = rnf mset `seq` rnf ft
instance (NFData a) => NFData (ForestMSet a) where
   rnf (ForestMSet nnodes forsize ft) = rnf nnodes `seq` rnf forsize `seq` rnf ft
instance (NFData a) => NFData (MultiSet a) where
   rnf (MultiSet e o f) = rnf e `seq` rnf o `seq` rnf f
instance (NFData a,NFData v) => NFData (FingerTree v a) where
   rnf Empty      = seq Empty ()
   rnf (Single x) = seq x ()
   rnf (Deep set pr mid sf) = rnf set `seq` rnf pr `seq` rnf mid `seq` rnf sf
instance (NFData a) => NFData (Digit a) where
   rnf (One a) = seq a ()
   rnf (Two a b) = rnf a `seq` rnf b
   rnf (Three a b c) = rnf a `seq` rnf b `seq` rnf c
   rnf (Four a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf c `seq` rnf d
instance (NFData a,NFData v) => NFData (Node v a) where
   rnf (Node2 v a b) = rnf v `seq` rnf a `seq` rnf b
   rnf (Node3 v a b c) = rnf v `seq` rnf a `seq` rnf b `seq` rnf c


emptyTreeMSet :: Integral a => TreeMSet a -- Integral a => FingerTree (MultiSet a) (MSet a)
emptyTreeMSet = empty


affixes :: ForestMSet a -> ([String],String,[String])
affixes (ForestMSet _ _ forest) = affixes' forest ([],"",[])

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


t1,t2,t8,t9 :: TreeMSet Int
t1 = foldr (<|) empty (map MSet [(1,1),(1,2),(2,2),(1,2),(1,3),(3,3),(1,3),(1,4),(4,4),(1,4)])
t2 = foldr (<|) empty (map MSet [(5,5),(5,6),(6,6),(6,7),(7,7),(6,7),(5,6)])
t8 = MSet (8,8) <| empty ; t9 = MSet (9,9) <| empty

f1 :: ForestMSet Int
f1 = ForestMSet 7 (s1+s2) ft
 where
   ft = (t1 <| empty) |> t2
   s1 = S.size (getEvens (measure t1)) + S.size (getOdds (measure t1)) + S.size (getEdges (measure t1))
   s2 = S.size (getEvens (measure t2)) + S.size (getOdds (measure t2)) + S.size (getEdges (measure t2))

f2 = ForestMSet nnodes forestsize ft
 where
   ft    = foldr (<|) empty [t9,t1,t2,t8]
   nnodes = sum $  map (S.size . getEvens . measure) [t9,t1,t2,t8]
                ++ map (S.size . getOdds  . measure) [t9,t1,t2,t8]
   forestsize  = nnodes + sum ( map (S.size . getEdges . measure) [t9,t1,t2,t8] )

f = linkMSet 8 3 $ linkMSet 9 8 $ linkMSet 6 3 f2
g = cutMSet  6 3 $ cutMSet  3 1 f

searchMSet :: (Integral a, Measured (MultiSet a) b) =>
              (a,a) -> FingerTree (MultiSet a) b -> SearchResult (MultiSet a) b
searchMSet p@(x,y) ftree
  | x==y && even x = let predicate setx _ = (S.member x) (getEvens setx) in search predicate ftree
  | x==y && odd  x = let predicate setx _ = (S.member x) (getOdds  setx) in search predicate ftree
  | otherwise      = let predicate setx _ = (S.member p) (getEdges setx) in search predicate ftree

searchEdgeMSet :: (Integral a, Measured (MultiSet a) b)
              => (a,a)
              -> FingerTree   (MultiSet a) b
              -> SearchResult (MultiSet a) b
searchEdgeMSet (u,v) ftree =
  let predicate setx _ = (S.member pair) (getEdges setx)
      pair = (min u v, max u v)
  in  search predicate ftree


instance Ord a => Semigroup (MultiSet a) where
  (MultiSet x1 y1 z1) <> (MultiSet x2 y2 z2) = MultiSet (S.union x1 x2) (S.union y1 y2) (S.union z1 z2)
instance Ord a => Monoid (MultiSet a) where
  mempty = MultiSet S.empty S.empty S.empty

instance (Integral a, Ord a) => Measured (MultiSet a) (MSet a) where
   measure (MSet p) = whichSet p

whichSet p@(x,y)
  | x==y && even x = MultiSet (S.insert x S.empty) S.empty             S.empty
  | x==y && odd  x = MultiSet S.empty             (S.insert x S.empty) S.empty
  | otherwise      = MultiSet S.empty              S.empty            (S.insert p S.empty)


evens p@(x,y) = if (x==y) && (even x) then S.insert p S.empty else S.empty
odds  p@(x,y) = if (x==y) && (odd  x) then S.insert p S.empty else S.empty
edges p@(x,y) = if (x/=y) then S.insert p S.empty else S.empty


forestDet forest =
  let (ForestEF nnodes forestsize ft) = forest
      lefty  :< _      = viewl (getFT ft)
      _      :> righty = viewr (getFT ft)
      setx             = measure (getFT ft)
  in  putStrLn  ( "\nNodes= "++show nnodes
                ++"\nSize=  "++show forestsize++" (nodes+edges)\n"
                ++"\nLeftmost  (1st)  leaf= "++show lefty++"\n"
                ++"\nRightmost (last) leaf= "++show righty++"\n"
                ++"\nNodes union Edges    = "++show setx++"\n")

forestMSetDet forest =
  let (ForestMSet nnodes forestsize ft) = forest
      lefty  :< _      = viewl ft
      _      :> righty = viewr ft
      setx             = measure  ft
      evens            = getEvens setx
      odds             = getOdds  setx
      edges            = getEdges setx
  in  putStrLn  ( "\nNodes= "++show nnodes
                ++"\nSize=  "++show forestsize++" (nodes+edges)\n"
                ++"\nLeftmost  (1st)  leaf= "++show lefty++"\n"
                ++"\nRightmost (last) leaf= "++show righty++"\n"
                ++"\nNodes (evens)        = "++show evens++"\n"
                ++"\nNodes (odds)         = "++show odds++"\n"
                ++"\nEdges                = "++show edges++"\n")

emptyForest :: Ord a => ForestEF a
emptyForest  = ForestEF 0 0 (FT empty)

emptyForestMSet :: Integral a => ForestMSet a
emptyForestMSet  = ForestMSet 0 0 empty

emptyTree :: Ord a => TreeEF a
emptyTree  = empty

instance (Ord a) => Measured (S.Set (a,a)) (a,a) where
   measure (x,y) = S.insert (x, y) S.empty

instance Show a => Show (ForestEF a) where
  show (ForestEF v s ft) =
    "FT " ++ show v ++ " " ++ show s ++ "\n" ++ show (getFT ft)

-- ----------------------------------------------------------
--           ROOT of tree

root :: Ord a => TreeEF a -> Maybe a
root tree = case viewl tree of
  EmptyL   -> Nothing
  x :< _   -> Just $ fst x

rootMSet :: Integral a => TreeMSet a -> Maybe a
rootMSet tree = case viewl tree of
  EmptyL      -> Nothing
  MSet x :< _ -> Just $ fst x

-- ----------------------------------------------------------
--           REroot of tree

reroot :: Ord a => TreeEF a -> a -> TreeEF a
reroot tree vertex = case (search pred tree) of
   Position left _ right -> root <| (right >< left)
   _                     -> tree
 where root          = (vertex,vertex)
       pred before _ = (S.member root) before

rerootMSet :: Integral a => TreeMSet a -> a -> TreeMSet a
rerootMSet tree vertex = case (searchMSet (vertex,vertex) tree) of
  Position left _ right -> (MSet (vertex,vertex)) <| (right >< left)
  _                     -> tree

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--            CONNECTED ( u, v ) in same tree ? IN WHICH tour ?

nodeIn :: Ord a => a -> ForestEF a -> Maybe (TreeEF a, a)
nodeIn v (ForestEF _ _ ft) =
 case search pred (getFT ft) of
  Position _ tree _ ->
     case (root tree) of
       Nothing    -> Nothing
       Just rootT -> Just (tree, rootT)
  _                 -> Nothing     -- node v not in forest
 where
   pred before _ = (S.member (v,v)) before

nodeInMSet :: Integral a => a -> ForestMSet a -> Maybe (TreeMSet a, a)
nodeInMSet v (ForestMSet _ _ ft) =
  case (searchMSet (v,v) ft) of
    Position _ tree _ ->
      case (rootMSet tree) of
        Nothing    -> Nothing
        Just rootT -> Just (tree, rootT)
    _              -> Nothing     -- node v not in forest
{-
edgeInMSet :: Integral a
            => (a,a)
            -> ForestMSets a
            -> Maybe (TreeMSets a, a, ForestMSets a, ForestMSets a)
-}
edgeInMSet pair (ForestMSet _ _ ft) =
  case (searchEdgeMSet pair ft) of
    Position left tree right -> Just (tree,left,right)
--      case (rootMSet tree) of
--        Nothing    -> Nothing
--        Just rootT -> Just (tree, rootT,left,right)
    _                 -> Nothing

-- | MAIN function regarding connectivity topic
-- | Are these two nodes connected in the provided forest?

connBool :: Ord a => a -> a -> ForestEF a -> Bool
connBool x y f =
 case (nodeIn x f, nodeIn y f) of
  (Nothing          , _           ) -> False
  (_                , Nothing     ) -> False
  (Just (tx,rx)     , Just (ty,ry)) -> if rx == ry  then True else False

connBoolMSet :: Integral a => a -> a -> ForestMSet a -> Bool
connBoolMSet x y f@(ForestMSet _ _ ft) =
--  case (searchMSet (x,y) ft) of
--    Position _ _ _ -> True
--    _              ->
     case (nodeInMSet x f, nodeInMSet y f) of
      (Nothing          , _           ) -> False
      (_                , Nothing     ) -> False
      (Just (_,rx)     , Just (_,ry)) ->
       if rx == ry  then True else False
{-
conn' :: Ord a => a -> a -> ForestEF a -> ForestEF a
conn' x y f =
 case (nodeIn x f, nodeIn y f) of
  (Nothing          , _           ) -> f
  (_                , Nothing     ) -> f
  (Just (tx,rx)     , Just (ty,ry)) ->
                     if rx == ry  then f
                                  else f

-- ASKs for connectivity provided a [random] node list
-- applied to a [random] forest to get a list of results
connList :: Ord a => [a] -> ForestEF a -> [Bool]
connList []       f = []
connList [x]      f = []
connList (x:y:ys) f = conn x y f : connList ys f
-}

connected :: Ord a => a -> a -> ForestEF a
          -> (Bool, Maybe (TreeEF a, a, TreeEF a, a) )
connected x y f =
 case (nodeIn x f, nodeIn y f) of
  (Nothing          , _           ) -> (False, Nothing)
  (_                , Nothing     ) -> (False, Nothing)
  (Just (tx,rx)     , Just (ty,ry)) -> if rx == ry
                                   then (True,  Just(tx,rx,tx,rx))
                                   else (False, Just(tx,rx,ty,ry))


connectedMSet :: Integral a => a -> a -> ForestMSet a
              -> (Bool, Maybe (TreeMSet a, a, TreeMSet a, a) )
connectedMSet x y f =
  case (nodeInMSet x f, nodeInMSet y f) of
    (Nothing          , _           ) -> (False, Nothing)
    (_                , Nothing     ) -> (False, Nothing)
    (Just (tx,rx)     , Just (ty,ry)) ->
     if rx == ry then (True,  Just(tx,rx,tx,rx)) else (False, Just(tx,rx,ty,ry))


-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--                           L I N K  (for trees and forest)

-- Safe? this link is for cases when it is called off the forest
linkTreeSafe :: Ord a => a -> TreeEF a -> a -> TreeEF a -> Maybe (TreeEF a)
linkTreeSafe u tu v tv = case (edgeIn (u,u) tu, edgeIn (v,v) tv) of
  (False, _    ) -> Nothing
  (_    , False) -> Nothing
  (True , True ) -> Just $
    let from = reroot tu u
        (Position left _ right) = search pred tv
    in  ((left |> (v,v)) |> (v,u)) >< from >< ((u,v) <| right)
 where
   pred before _ = (S.member (v,v)) before

-- this is a trusted link, because it is called from LINK, so checks where done
linkTree :: Ord a => a -> TreeEF a -> a -> TreeEF a -> Maybe (TreeEF a)
linkTree u tu v tv = Just $
    let from = reroot tu u
        (Position left _ right) = search pred tv
    in  ((left |> (v,v)) |> (v,u)) >< from >< ((u,v) <| right)
 where
   pred before _ = (S.member (v,v)) before


linkUnsafeTreeMSet :: -- unsafe is 'safe' when used after the forest's link verifications
   Integral a => a
              -> TreeMSet a
              -> a
              -> TreeMSet a
              -> TreeMSet a
linkUnsafeTreeMSet u tu v tv =
   let from = rerootMSet tu u
       (Position left _ right) = searchMSet (v,v) tv
       pair = (min u v,max u v)
   in ((left |> MSet (v,v)) |> MSet pair) >< from >< (MSet pair <| right)

-- for a really DENSE forest (trend is few giant trees)
linkDEN :: Ord a => a -> a -> ForestEF a -> ForestEF a
linkDEN x y forest@(ForestEF nnodes size ft) =
    if (edgeIn (x,y) f) then forest
    else
     case connected x y forest of
      (False, Just (tx,rx,ty,ry)) -> case (linkTree x tx y ty) of
         Nothing     -> forest
         Just result -> linkAll result
      _                           -> forest
 where
    f                  = getFT ft
    Position lf' _ rf' = search predX f
    Position lf  _ rf  = search predY (lf' >< rf')
    linkAll tree    = ForestEF nnodes (size+2) (FT $ tree <| (lf >< rf) )
    predX before _ = (S.member (x,x)) before
    predY before _ = (S.member (y,y)) before

linkDenMSet :: Integral a => a -> a -> ForestMSet a -> ForestMSet a
linkDenMSet x y forest@(ForestMSet nnodes size ft) =
    case (searchEdgeMSet (x,y) ft) of
      Position _ _ _ -> forest
      _ ->
        case connectedMSet x y forest of
          (False, Just(tx,rx,ty,ry)) -> linkAll (linkUnsafeTreeMSet x tx y ty)
          _                          -> forest
  where
    linkAll tree = ForestMSet nnodes (size+1) (tree <| (lf >< rf))
    Position lf' _ rf' = searchMSet (x,x) ft
    Position lf  _ rf  = searchMSet (y,y) (lf' >< rf')

linkAvgMSet :: Integral a => a -> a -> ForestMSet a -> ForestMSet a
linkAvgMSet x y forest@(ForestMSet nnodes size ft) =
      case connectedMSet x y forest of
        (False, Just(tx,rx,ty,ry)) -> linkAll (linkUnsafeTreeMSet x tx y ty)
        _                          -> forest
  where
    linkAll tree = ForestMSet nnodes (size+1) (tree <| (lf >< rf))
    Position lf' _ rf' = searchMSet (x,x) ft
    Position lf  _ rf  = searchMSet (y,y) (lf' >< rf')

treesize tree =
  let sets = measure tree
  in  (S.size (getEvens sets))+(S.size (getOdds sets))

linkTreeMSetMax maxi u tu v tv
  | (treesize tu) + (treesize tv) > maxi = Nothing
  | otherwise = Just $
      let from = rerootMSet tu u
          (Position left _ right) = searchMSet (v,v) tv
          pair = (min u v,max u v)
      in  ((left |> MSet (v,v)) |> MSet pair) >< from >< (MSet pair <| right)

linkMSetMax :: Integral a => Int -> a -> a -> ForestMSet a -> ForestMSet a
linkMSetMax maxi x y forest@(ForestMSet nnodes size ft) =
      case connectedMSet x y forest of
        (False, Just(tx,rx,ty,ry)) -> case (linkTreeMSetMax maxi x tx y ty) of
                                           Just tree -> linkAll tree
                                           Nothing   -> forest
        _                          -> forest
  where
    linkAll tree = ForestMSet nnodes (size+1) (tree <| (lf >< rf))
    Position lf' _ rf' = searchMSet (x,x) ft
    Position lf  _ rf  = searchMSet (y,y) (lf' >< rf')



-- for an AVERAGE and SPARSE forest
linkAVG :: Ord a => a -> a -> ForestEF a -> ForestEF a
linkAVG x y forest@(ForestEF nnodes size ft) =
     case connected x y forest of
      (False, Just (tx,rx,ty,ry)) -> case (linkTree x tx y ty) of
         Nothing     -> forest
         Just result -> linkAll result
      _                           -> forest
 where
    f                  = getFT ft
    Position lf' _ rf' = search predX f
    Position lf  _ rf  = search predY (lf' >< rf')
    linkAll tree    = ForestEF nnodes (size+2) (FT $ tree <| (lf >< rf) )
    predX before _ = (S.member (x,x)) before
    predY before _ = (S.member (y,y)) before


link :: Ord a => a -> a -> ForestEF a -> ForestEF a
link x y forest@(ForestEF nnodes size ft)
  | x == y            = forest
  | size > (2*nnodes) = linkDEN x y forest
  | otherwise         = linkAVG x y forest


linkMSet :: Integral a => a -> a -> ForestMSet a -> ForestMSet a
linkMSet x y forest@(ForestMSet nnodes size _)
  | x == y            = forest
  | size > (2*nnodes) = linkAvgMSet x y forest -- linkDenMSet x y forest
  | otherwise         = linkAvgMSet x y forest

linkMax :: Integral a => Int -> a -> a -> ForestMSet a -> ForestMSet a
linkMax n x y forest@(ForestMSet nnodes size _)
  | x == y            = forest
--  | size > (2*nnodes) = linkAvgMSet x y forest -- linkDenMSet x y forest
  | otherwise         = linkMSetMax n x y forest


-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--                             C U T   (for trees and forest)
--

cutTree :: Ord a => a -> a -> TreeEF a -> Maybe (TreeEF a,TreeEF a)
cutTree u v tree = case search predUV tree of
 Position left _ right ->
   case (search predVU left ) of
      Position leftL _ rightL ->           -- (v,u) is on the left
        Just (rightL, leftL >< right)
      _              ->                    -- (v,u) is on the right
        case (search predVU right) of
          Position leftR _ rightR ->
            Just (leftR, left >< rightR)
          _ -> Nothing -- >>>> BAD Formed tree since (v,u) is missing
 _  -> Nothing  -- >>>>>>>>> BAD Formed tree since (u,v) is missing
 where
   predUV before _ = (S.member (u,v)) before
   predVU before _ = (S.member (v,u)) before

cutUnsafeTreeMSet :: -- unsafe is 'safe' when used after the forest's CUT verifications
   Integral a => a
              -> a
              -> TreeMSet a
              -> (TreeMSet a,TreeMSet a)
cutUnsafeTreeMSet u v tree = case searchEdgeMSet pair tree of
    Position left _ right ->
      case (searchEdgeMSet pair left) of
        Position leftL _ rightL -> (rightL, leftL >< right)
        _                       ->
          case (searchEdgeMSet pair right) of
            Position leftR _ rightR -> (leftR,  left  >< rightR)
            _                       -> undefined -- error "Tree malformed for "++show v++" and "++show u
    _                     -> undefined -- error "Tree malformed for "++show u++" and "++show v
 where
   pair = (min u v, max u v)


cutMSet :: Integral a => a -> a -> ForestMSet a -> ForestMSet a
cutMSet x y forest@(ForestMSet nnodes size ft) =
    case edgeInMSet (x,y) forest of
      Nothing                 -> forest
      Just (tree,ltFor,rtFor) -> buildForest (cutUnsafeTreeMSet x y tree) ltFor rtFor
  where
    buildForest (leftTree,rightTree) lFor rFor
       = ForestMSet nnodes (size - 1) (leftTree <| rightTree <| (lFor >< rFor))

cutMin :: Integral a => Int -> a -> a -> ForestMSet a -> ForestMSet a
cutMin n x y forest@(ForestMSet nnodes size ft) =
   case edgeInMSet (x,y) forest of
     Nothing                 -> forest
     Just (tree,ltFor,rtFor) -> case (cutTreeMSetMin n x y tree) of
           Just (lefty, righty) -> buildForest (lefty,righty) ltFor rtFor
--           buildForest (cutUnsafeTreeMSet x y tree) ltFor rtFor
           Nothing -> forest
 where
   buildForest (leftTree,rightTree) lFor rFor
      = ForestMSet nnodes (size - 1) (leftTree <| rightTree <| (lFor >< rFor))



cutTreeMSetMin mini u v tree = case searchEdgeMSet pair tree of
   Position left _ right ->
     case (searchEdgeMSet pair left) of
       Position leftL _ rightL ->
           case ((treesize rightL)>=mini, (treesize (leftL >< right))>= mini ) of
              (False, _)  -> Nothing
              (_, False)  -> Nothing
              (True,True) -> Just $ (rightL,  leftL  >< right)
       _                       ->
         case (searchEdgeMSet pair right) of
           Position leftR _ rightR ->
                 case ((treesize leftR)>=mini, (treesize (left >< rightR))>= mini ) of
                   (False, _)  -> Nothing
                   (_, False)  -> Nothing
                   (True,True) -> Just $ (leftR,  left  >< rightR)
           _                       -> undefined -- error "Tree malformed for "++show v++" and "++show u
   _                     -> undefined -- error "Tree malformed for "++show u++" and "++show v
  where
     pair = (min u v, max u v)



-- for really DENSE forest (trend is few giant trees)
cutDEN :: Ord a => a -> a -> ForestEF a -> ForestEF a
cutDEN x y forest@(ForestEF nnodes size ft) =
    if not exists then forest
    else
      case tree of
        Nothing  -> forest
        Just tx  -> case (cutTree x y tx) of
          Nothing     -> forest
          Just result -> buildForest result
 where
    f                   = getFT ft
    buildForest (t2,t3) = ForestEF nnodes (size - 2) (FT $ t2 <| (t3 <| (lf >< rf)) )
    Position lf _ rf    = search pred f
    pred before _       = (S.member (x,x)) before
    (exists,tree)       = edgeInWith (x,y) forest
-- for AVERAGE and SPARSE forest
cutAVG :: Ord a => a -> a -> ForestEF a -> ForestEF a
cutAVG x y forest@(ForestEF nnodes size ft) =
    if not (edgeIn (x,y) f) then forest
    else
      case tree of
        Nothing -> forest
        Just tx -> case (cutTree x y tx) of
          Nothing     -> forest
          Just result -> buildForest result
 where
    f                   = getFT ft
    buildForest (t2,t3) = ForestEF nnodes (size - 2) (FT $ t2 <| (t3 <| (lf >< rf)) )
    Position lf _ rf    = search pred f
    pred before _       = (S.member (x,x)) before
    (_,tree)            = edgeInWith (x,y) forest


cut :: Ord a => a -> a -> ForestEF a -> ForestEF a
cut x y forest@(ForestEF nnodes size ft)
  | x == y            = forest
  | size > (2*nnodes) = cutDEN x y forest
  | otherwise         = cutAVG x y forest

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--            HELPER functions  (for trees and forest)
--

-- Test membership of an edge (or vertex when (x,x)) in monoidal set
-- First type 'a' is usually a pair, 'b' can be either a pair or a tree
edgeIn :: (Measured (S.Set a) b, Ord a) =>
          a
          -> FingerTree (S.Set a) b
          -> Bool
edgeIn e setFT = case (search pred setFT) of
  Position _ _ _ -> True
  _              -> False
 where
   pred before _ = (S.member e) before

{-

edgeInMSet :: (Integral a, Measured (MultiSet a) b)
           => (a,a)
           -> FingerTree (MultiSet a) b --ForestMSets a -> Bool
           -> Bool
edgeInMSet p ft = case (searchEdgeMSet p ft) of
  Position _ _ _ -> True
  _              -> False

-}

-- Test membership of an edge (or vertex when (x,x)) in monoidal set
-- ALSO return the second value in the search: the corresponding tree
-- First type 'a' is usually a pair, 'b' can be either a pair or a tree
edgeInWith :: (Ord a)
           => (a,a)
           -> ForestEF a
           -> (Bool, Maybe (TreeEF a))
edgeInWith e (ForestEF _ _ ft) = case (search pred setFT) of
  Position _ x _ -> (True  , Just x )
  _              -> (False , Nothing)
 where
   pred before _ = (S.member e) before
   setFT         = getFT ft

pf :: (Integral a,Show a) => ForestMSet a -> IO ()
pf = prtForMSet
pt :: (Integral a,Show a) => TreeMSet a -> IO ()
pt = prtTreeMSet
tlt :: (Integral a) => TreeMSet a -> [(a,a)]
tlt = toListTreeMSet
tlf :: (Integral a) => ForestMSet a -> [(a,a)]
tlf = toListForMSet

sizeForest (ForestMSet _ _ Empty  ) = 0
sizeForest (ForestMSet _ sf ft    ) = sf
nnodesForest (ForestMSet _ _ Empty) = 0
nnodesForest (ForestMSet nn _ ft  ) = nn

toListFT :: (Ord a, Measured v a) => FingerTree v a -> [a]
toListFT ft = case (viewl ft) of
  EmptyL    -> []
  x :< rest -> x : toListFT rest

toListTreeMSet :: Integral a => TreeMSet a -> [(a,a)]
toListTreeMSet ft = case (viewl ft) of
  EmptyL    -> []
  (MSet x) :< rest -> x : toListTreeMSet rest

toListForMSet :: Integral a => ForestMSet a -> [(a,a)]
toListForMSet (ForestMSet _ _ ft) = concat $ toListForMSet' ft

toListForMSet' ft = case (viewl ft) of
  EmptyL  -> []
  x :< xs -> toListTreeMSet x : toListForMSet' xs

prtForMSet :: (Integral a, Show a) => ForestMSet a -> IO ()
prtForMSet (ForestMSet _ _ ft) = ( prtForest . ets2rf ) ( forest ft )
 where
    forest g = case viewl g of
      EmptyL   -> []
      x :<  xs -> toListTreeMSet x : forest xs

prtTreeMSet :: (Eq a, Integral a, Show a) => TreeMSet a -> IO ()
prtTreeMSet  = prtTree . et2rt . toListTreeMSet

prtTOUR :: (Ord a, Show a) => TreeEF a -> IO ()
prtTOUR  = prtTree . et2rt . toListFT

prtMTOUR :: (Ord a, Show a) => Maybe (TreeEF a) -> IO ()
prtMTOUR  met = case met of
  Nothing -> putStrLn "\n No tree generated \n"
  Just et -> prtTree . et2rt . toListFT $ et

prtFOREST :: (Ord a, Show a) => ForestEF a -> IO ()
prtFOREST (ForestEF _ _ ft) = ( prtForest . ets2rf ) ( forest f )
 where
   f        = getFT ft
   forest f = case viewl f of
     EmptyL   -> []
     x :<  xs -> toListFT x : forest xs



data Elem a = Elem a

instance Show a => Show (Elem a) where
 show (Elem a) = show a

type FTS a = FingerTree (S.Set a) (Elem a)

instance Ord a => Measured (S.Set a) (Elem a) where
  measure (Elem x) = S.insert x S.empty

emptyFTS :: Ord a => FTS a
emptyFTS =  empty

toListFTS :: Ord a => FTS a -> [a]
toListFTS ft = case (viewl ft) of
 EmptyL    -> []
 Elem x :< rest -> x : toListFTS rest


-- -------------------------------------------------------------------------
--
--    SOME TREE AND FOREST EXAMPLES
path  = "/Users/jcsaenzc/Documents/H/TXTs/rndseed-11/"
pffull   n = path ++ "forestsUnik/full/fullLinksUnit-"++show n++"-11.txt"
pft10    n = path ++ "forestsUnik/t10/forestT10-"++show n++"-11.txt"
pft2     n = path ++ "forestsUnik/t2/forestT2-"++show n++"-11.txt"
pfunit   n = path ++ "forestsUnik/unit/unit-"++show n++"-11.txt"
pflct10s n s = path ++ "forestsUnik/t10/forestLinkCutsT10sizes-"++show n++"-"++show s++"-11.txt"
pcutfull n = path ++ "cutList/full/cutsFullUnit-"++show n++"-11.txt"
pcutt10  n = path ++ "cutList/t10/cutsT10-"++show n++"-11.txt"
pcutt2   n = path ++ "cutList/t10/cutsT2-"++show n++"-11.txt"
plinkuni n = path ++ "linkList/unit/linksUnit-"++show n++"-11.txt"
plinkt10 n = path ++ "linkList/t10/linksT10-"++show n++"-11.txt"
plinkt2  n = path ++ "linkList/t2/linksT2-"++show n++"-11.txt"
plinkcut n s = path ++ "linkcutList/t10/linkcutsT10-"++show n++"-"++show s++"-11.txt"
plinkcuts n s = path ++ "linkcutList/t10/linkcutsT10sizes-"++show n++"-"++show s++"-11.txt"

minmax :: [(Int,Int)] -> [(Int,Int)]
minmax ft = map (\(x,y)->(min x y, max x y)) ft

midList :: Int -> [(Int,Int)] -> [(Int,Int)]
midList n l =
  let len = div (length l) 2
      l'  = drop (len -(div n 2)) l
  in  take n l'

ft1,ft2,s8,s9 :: TreeEF Int
ft1 = foldr (<|) emptyTree [(1,1),(1,2),(2,2),(2,1),(1,3),(3,3),(3,1),(1,4),(4,4),(4,1)]
ft2 = foldr (<|) emptyTree [(5,5),(5,6),(6,6),(6,7),(7,7),(7,6),(6,5)]
s8 = (8,8) <| emptyTree ; s9 = (9,9) <| emptyTree

forest1,forest2 :: ForestEF Int
forest1 = ForestEF 7 (s1+s2) ft
 where
   ft = FT $ (ft1 <| empty) |> ft2
   s1 = S.size $ measure ft1
   s2 = S.size $ measure ft2

forest2 = ForestEF 9 sizex ft
 where
   ft    = FT $ foldr (<|) empty [s9,ft1,ft2,s8]
   sizex = sum $ map (S.size . measure) [s9,ft1,ft2,s8]
{-
1.- forest3 = link 8 6 $ link 9 8 $ link 6 3 forest2  :: ForestEF Int
2.- ForestEF nn1 fs1 ff1 = forest3
3.- tx :< _ = viewl $ getFT ff1
3.- newf = ForestEF (nn1*2) (fs1*2) (FT ( getFT ff1 |> reverseTree id tx))
      1            4
      |            |
============       1
/     |     \      |
2     3     4      =====
      |           /     \
      6           3     2
      |           |
    =====         6
   /  |  \        |
   8  7  5      =====
   |           /  |  \
   9           5  7  8
               |
               9
-}
