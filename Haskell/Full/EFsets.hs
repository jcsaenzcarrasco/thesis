{-#  LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module EFsets where 

import FTfull -- Data.FingerTree
import qualified FTfull as FT -- Data.FingerTree as FT
import EtRt
import qualified Data.Set as S
import Data.Maybe (fromJust) 
import GHC.Exts (IsList(..))
import Data.Monoid ((<>))

-- ************************************************************
--    FT (Set (a,a))  (a,a)   

type TreeEF   a = FingerTree (S.Set (a,a)) (a,a)
type ForestEF a = FingerTree (S.Set (a,a)) (TreeEF a) 

emptyForest :: Ord a => ForestEF a  
emptyForest  = FT.empty 

emptyTree :: Ord a => TreeEF a 
emptyTree  = FT.empty 

instance (Ord a) => Edges (S.Set (a,a)) (a,a) where 
   edges (x,y) = S.insert (x, y) S.empty 

-- ----------------------------------------------------------
--           ROOT of tree 

root :: Ord a => TreeEF a -> Maybe a  
root tree = case viewl tree of
  EmptyL   -> Nothing
  x :< _   -> Just $ fst x

-- ----------------------------------------------------------
--           REroot of tree 

reroot' :: Ord a => TreeEF a -> a -> TreeEF a 
reroot' tree vertex = case (FT.search pred tree) of
   Position left _ right -> root <| (right >< left)
   _                     -> tree
 where root          = (vertex,vertex)
       pred before _ = (S.member root) before

reroot :: Ord a => TreeEF a -> a -> TreeEF a 
reroot tree vertex =
 let (left,right) = split pred tree
 in  right >< left
 where root        = (vertex,vertex)
       pred before = (S.member root) before

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
-- 
--            CONNECTED ( u, v ) in same tree ? IN WHICH tour ?

searchFor :: Ord a => a -> ForestEF a -> Maybe (TreeEF a, a) 
searchFor v f = 
 case FT.search pred f of 
  Position _ tree _ -> Just (tree, fromJust $ root tree) 
  _                 -> Nothing
 where
   pred before _ = (S.member (v,v)) before 

type PairTreeVertex a = (TreeEF a, a, TreeEF a, a) 

connected :: Ord a => a -> a -> ForestEF a -> (Bool, Maybe (PairTreeVertex a)) 
connected x y f = 
 case (searchFor x f, searchFor y f) of 
  (Nothing          , _           ) -> (False, Nothing) 
  (_                , Nothing     ) -> (False, Nothing) 
  (Just (tx,rx)     , Just (ty,ry)) -> if rx == ry 
                                   then (True,  Just(tx,rx,tx,rx))  
                                   else (False, Just(tx,rx,ty,ry))  
-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--                           L I N K  (for trees and forest)
--

linkTree' :: Ord a => a -> TreeEF a -> a -> TreeEF a -> Maybe (TreeEF a) 
linkTree' u tu v tv = case (pairIn (u,u) tu, pairIn (v,v) tv) of
  (False, _    ) -> Nothing
  (_    , False) -> Nothing 
  (True , True ) -> Just $
    let from = reroot tu u
        (Position left _ right) = FT.search pred tv
    in  ((left |> (v,v)) |> (v,u)) >< from >< ((u,v) <| right)
 where
   pred before _ = (S.member (v,v)) before

linkTree :: Ord a => a -> TreeEF a -> a -> TreeEF a -> Maybe (TreeEF a) 
linkTree u tu v tv = case (pairIn (u,u) tu, pairIn (v,v) tv) of
  (False, _    ) -> Nothing
  (_    , False) -> Nothing 
  (True , True ) -> Just $
    let from = reroot tu u
        (Position left _ right) = FT.search pred tv
    in  ((left |> (v,v)) |> (v,u)) >< from >< ((u,v) <| right)
 where
   pred before _ = (S.member (v,v)) before

link :: Ord a => a -> a -> ForestEF a -> ForestEF a 
link x y f 
  | x == y    = f  -- FURTHER MSG management
  | otherwise = 
     case connected x y f of 
      (False, Just (tx,rx,ty,ry)) -> case (linkTree x tx y ty) of
         Nothing     -> f
         Just result -> linkAll result 
      _                           -> f 
 where 
    Position lf' _ rf' = FT.search predX f 
    Position lf  _ rf  = FT.search predY (lf' >< rf') 
    linkAll tree    = tree <| (lf >< rf)
    predX before _ = (S.member (x,x)) before 
    predY before _ = (S.member (y,y)) before 

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--                             C U T   (for trees and forest)
--

cutTree :: Ord a => a -> a -> TreeEF a -> Maybe (TreeEF a,TreeEF a) 
cutTree u v tree = case FT.search predUV tree of
 Position left _ right ->
   case (FT.search predVU left ) of
      Position leftL _ rightL ->           -- (v,u) is on the left 
        Just (rightL, leftL >< right)
      _              ->                    -- (v,u) is on the right
        case (FT.search predVU right) of
          Position leftR _ rightR ->
            Just (leftR, left >< rightR)
          _ -> Nothing -- >>>> BAD Formed tree since (v,u) is missing 
 _  -> Nothing  -- >>>>>>>>> BAD Formed tree since (u,v) is missing     
 where
   predUV before _ = (S.member (u,v)) before    -- (u,v)
   predVU before _ = (S.member (v,u)) before    -- (v,u) 


cut :: Ord a => a -> a -> ForestEF a -> ForestEF a 
cut x y f  
 | x == y    = f  -- further notice about NOT cut computed 
 | otherwise = 
    case connected x y f of 
      (True, Just (tx,_,_,_)) -> case (cutTree x y tx) of
        Nothing     -> f 
        Just result -> buildForest result  
      _                       -> f -- further notice NOT cut ... 
 where 
    buildForest (t2,t3) = t2 <| (t3 <| (lf >< rf)) 
    Position lf _ rf = FT.search pred f
    pred before _    = (S.member (x,x)) before


-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--            HELPER functions  (for trees and forest)
--

pairIn :: (Edges (S.Set a) a, Ord a)
       => a -> FingerTree (S.Set a) a
       -> Bool
pairIn p monFT = case (FT.search pred monFT) of
  Position _ _ _ -> True 
  _              -> False
 where
   pred before _ = (S.member p) before 


nodeInF :: Ord a => (a,a) -> ForestEF a -> Bool
nodeInF x monFT = case (FT.search pred monFT) of
  Position _ _ _ -> True
  _              -> False
 where pred before _ = (S.member x) before 

toListFT :: (Ord a, Edges v a) => FingerTree v a -> [a]
toListFT ft = case (viewl ft) of
  EmptyL    -> []
  x :< rest -> x : toListFT rest


prtTOUR :: (Ord a, Show a) => TreeEF a -> IO () 
prtTOUR  = prtTree . et2rt . toListFT

prtMTOUR :: (Ord a, Show a) => Maybe (TreeEF a) -> IO () 
prtMTOUR  met = case met of
  Nothing -> putStrLn "\n No tree generated \n"
  Just et -> prtTree . et2rt . toListFT $ et 

prtFOREST :: (Ord a, Show a) => ForestEF a -> IO () 
prtFOREST f = ( prtForest . ets2rf ) ( forest f )
 where
   forest f = case viewl f of
     EmptyL   -> []
     x :<  xs -> toListFT x : forest xs 

-- -------------------------------------------------------------------------  
--
--    SOME TREE AND FOREST EXAMPLES 

ft1,ft2,s8,s9 :: TreeEF Int
ft1 = foldr (<|) emptyTree [(1,1),(1,2),(2,2),(2,1),(1,3),(3,3),(3,1),(1,4),(4,4),(4,1)] 
ft2 = foldr (<|) emptyTree [(5,5),(5,6),(6,6),(6,7),(7,7),(7,6),(6,5)]
fth = foldr (<|) emptyTree [(1,1),(1,2),(2,2),(2,5),(5,5),(5,14),(14,14),(14,26),(26,26),(26,14),(14,27),(27,27),(27,14),(14,5),(5,2)
                           ,(2,6),(6,6),(6,15),(15,15),(15,28),(28,28),(28,15),(15,6),(6,16),(16,16),(16,29),(29,29),(29,16),(16,6),(6,2)
                           ,(2,7),(7,7),(7,30),(30,30),(30,7),(7,31),(31,31),(31,7),(7,32),(32,32),(32,7),(7,2),(2,1)
                           ,(1,3),(3,3),(3,8),(8,8),(8,17),(17,17),(17,33),(33,33),(33,17),(17,8),(8,18),(18,18),(18,34),(34,34),(34,18),(18,8),(8,3)
                           ,(3,9),(9,9),(9,19),(19,19),(19,35),(35,35),(35,19),(19,36),(36,36),(36,19),(19,9),(9,3),(3,1)
                           ,(1,4),(4,4),(4,10),(10,10),(10,20),(20,20),(20,10),(10,21),(21,21),(21,10),(10,4)
                           ,(4,11),(11,11),(11,22),(22,22),(22,11),(11,4)
                           ,(4,12),(12,12),(12,23),(23,23),(23,12),(12,24),(24,24),(24,12),(12,4)
                           ,(4,13),(13,13),(13,25),(25,25),(25,37),(37,37),(37,25),(25,13),(13,4),(4,1)]
fth'= foldr (<|) emptyTree [(1,1),(1,2),(2,2),(2,5),(5,5),(5,14),(14,14),(14,26),(26,26),(14,26),(14,27),(27,27),(14,27),(5,14),(2,5)
                           ,(2,6),(6,6),(6,15),(15,15),(15,28),(28,28),(15,28),(6,15),(6,16),(16,16),(16,29),(29,29),(16,29),(6,16),(2,6)
                           ,(2,7),(7,7),(7,30),(30,30),(7,30),(7,31),(31,31),(7,31),(7,32),(32,32),(7,32),(2,7),(1,2)
                           ,(1,3),(3,3),(3,8),(8,8),(8,17),(17,17),(17,33),(33,33),(17,33),(8,17),(8,18),(18,18),(18,34),(34,34),(18,34),(8,18),(3,8) 
                           ,(3,9),(9,9),(9,19),(19,19),(19,35),(35,35),(19,35),(19,36),(36,36),(19,36),(9,19),(3,9),(1,3)  
                           ,(1,4),(4,4),(4,10),(10,10),(10,20),(20,20),(10,20),(10,21),(21,21),(10,21),(4,10)     
                           ,(4,11),(11,11),(11,22),(22,22),(11,22),(4,11)  
                           ,(4,12),(12,12),(12,23),(23,23),(12,23),(12,24),(24,24),(12,24),(4,12)
                           ,(4,13),(13,13),(13,25),(25,25),(25,37),(37,37),(25,37),(13,25),(4,13),(1,4)]

s8 = (8,8) <| emptyTree ; s9 = (9,9) <| emptyTree

forest1,forest2,ffth,ffth' :: ForestEF Int
forest1 =  (ft1 <| FT.empty) |> ft2  
forest2 = foldr (<|) FT.empty [s9,ft1,ft2,s8]
ffth    = fth  <| FT.empty
ffth'   = fth' <| FT.empty 


ftc1,ftc2     :: TreeEF Char
ftc1 = foldr (<|) emptyTree [('c','c'),('c','a'),('a','a'),('a','c'),('c','e'),('e','e'),('e','c')
                            ,('c','b'),('b','b'),('b','c'),('c','d'),('d','d'),('d','c')]
f = 'f'; g = 'g'; h = 'h'; i = 'i'
ftc2 = foldr (<|) emptyTree [(f,f),(f,g),(g,g),(g,f),(f,h),(h,h),(h,f),(f,i),(i,i),(i,f)]


