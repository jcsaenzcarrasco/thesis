{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances, Safe, DeriveGeneric, AutoDeriveTypeable, FlexibleContexts #-}

module DataFingerTreeTOP where

import Prelude hiding (null, reverse)
import GHC.Generics
import qualified Prelude (null)
import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Data.Monoid
import Data.Maybe
import Data.Foldable (Foldable(foldMap))
import Data.Semigroup
import Data.Foldable (toList)
import qualified Data.Set as S
import EtRt
import Control.DeepSeq


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FingerTree
-- Copyright   :  (c) Ross Paterson, Ralf Hinze 2006
--
-- augmented to devise the source code in the thesis
-- "On the Implementation of Purely Functional Data Structures for the Linearisation case of Dynamic Trees"
-- by Juan C. Saenz-Carrasco
-----------------------------------------------------------------------------


infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

-- | View of the left end of a sequence.
data ViewL s a
    = EmptyL        -- ^ empty sequence
    | a :< s a      -- ^ leftmost element and the rest of the sequence
    deriving (Eq, Ord, Show, Read)

-- | View of the right end of a sequence.
data ViewR s a
    = EmptyR        -- ^ empty sequence
    | s a :> a      -- ^ the sequence minus the rightmost element,
                    -- and the rightmost element
    deriving (Eq, Ord, Show, Read)

instance (Measured v a) => Semigroup (FingerTree v a) where
    (<>) = (><)

-- | 'empty' and '><'.
instance (Measured v a) => Monoid (FingerTree v a) where
    mempty = empty
    mappend = (><)

-- Explicit Digit type (Exercise 1)

data Digit a
    = One a
    | Two a a
    | Three a a a
    | Four a a a a
    deriving (Generic,Read,Show)

instance Foldable Digit where
    foldMap f (One a) = f a
    foldMap f (Two a b) = f a `mappend` f b
    foldMap f (Three a b c) = f a `mappend` f b `mappend` f c
    foldMap f (Four a b c d) = f a `mappend` f b `mappend` f c `mappend` f d


-- | Things that can be measured.
class (Monoid v) => Measured v a | a -> v where
    measure :: a -> v

instance (Measured v a) => Measured v (Digit a) where
    measure = foldMap measure

data Node v a = Node2 !v a a | Node3 !v a a a
    deriving (Generic,Read,Show)

instance Foldable (Node v) where
    foldMap f (Node2 _ a b) = f a `mappend` f b
    foldMap f (Node3 _ a b c) = f a `mappend` f b `mappend` f c

node2        ::  (Measured v a) => a -> a -> Node v a
node2 a b    =   Node2 (measure a `mappend` measure b) a b

node3        ::  (Measured v a) => a -> a -> a -> Node v a
node3 a b c  =   Node3 (measure a `mappend` measure b `mappend` measure c) a b c

instance (Monoid v) => Measured v (Node v a) where
    measure (Node2 v _ _)    =  v
    measure (Node3 v _ _ _)  =  v

nodeToDigit :: Node v a -> Digit a
nodeToDigit (Node2 _ a b) = Two a b
nodeToDigit (Node3 _ a b c) = Three a b c


data FingerTree v a
    = Empty
    | Single a
    | Deep !v !(Digit a) (FingerTree v (Node v a)) !(Digit a)
    deriving (Generic,Read,Show)

newtype Leaf      a = Leaf (a,a)  deriving (Eq,Read,Show)
data    MultiSet  a = MultiSet {getEvens :: S.Set a, getOdds  :: S.Set a, getEdges :: S.Set (a,a)}  deriving (Eq,Read,Show)
data    TreeTop   a = TreeTop   (MultiSet a) (FingerTree (MultiSet a)(Leaf a))     deriving (Eq,Read,Show)
data    ForestTop a = ForestTop (MultiSet a) (FingerTree (MultiSet a)(TreeTop a))  deriving (Read,Show)

instance (Integral a) => Eq (ForestTop a) where
   ft1 == ft2 = minmax ft1 == minmax ft2

minmax ft = map (\(x,y)->(min x y, max x y)) (toListForestTop ft)

instance (NFData a) => NFData (Leaf a) where
   rnf (Leaf pair) = pair `seq` ()
instance (NFData a) => NFData (TreeTop a) where
   rnf (TreeTop mset ft) = rnf mset `seq` rnf ft
instance (NFData a) => NFData (ForestTop a) where
   rnf (ForestTop mset ft) = rnf mset `seq` rnf ft
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


deep ::  (Measured v a) =>
     Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deep pr m sf = Deep mempty pr m sf

-- | /O(1)/. The cached measure of a tree.
instance (Measured v a) => Measured v (FingerTree v a) where
    measure Empty           =  mempty
    measure (Single x)      =  measure x
    measure (Deep v _ _ _)  =  v

-- | Elements from left to right.
instance Foldable (FingerTree v) where
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Deep _ pr m sf) =
        foldMap f pr `mappend` foldMap (foldMap f) m `mappend` foldMap f sf

instance (Eq a) => Eq (FingerTree v a) where
    xs == ys = toList xs == toList ys

-- | Lexicographical order from left to right.
instance (Ord a) => Ord (FingerTree v a) where
    compare xs ys = compare (toList xs) (toList ys)

-- | /O(1)/. The empty sequence.
empty :: Measured v a => FingerTree v a
empty = Empty

-- | /O(1)/. A singleton sequence.
singleton :: Measured v a => a -> FingerTree v a
singleton = Single

-- | /O(n)/. Create a sequence from a finite list of elements.
-- The opposite operation 'toList' is supplied by the 'Foldable' instance.
fromList :: (Measured v a) => [a] -> FingerTree v a
fromList = foldr (<|) Empty

pMSet (MultiSet evens odds edges) =
   putStrLn $ "evens:["  ++showless (show evens)++ "]\nodds: [" ++showless (show odds)++ "]\nedges:["++showless (show edges)++"]\n"
 where
   showless :: [a] -> [a]
   showless str = let str' = drop 10 str in  init str' -- removes substring "fromList ["  and final character "]" from Sets

instance Ord a => Semigroup (MultiSet a) where
   (MultiSet mx1 my1 mz1) <> (MultiSet mx2 my2 mz2) = MultiSet (S.union mx1 mx2) (S.union my1 my2) (S.union mz1 mz2)

instance Ord a => Monoid (MultiSet a) where
   mempty =  emptyMSet
emptyMSet :: MultiSet a
emptyMSet =  MultiSet S.empty S.empty S.empty

instance (Integral a, Ord a) => Measured (MultiSet a) (Leaf a) where
   measure (Leaf p) = buildMSet p emptyMSet
buildMSet :: Integral a => (a, a) -> MultiSet a -> MultiSet a
buildMSet p@(x,y) (MultiSet evens odds edges)
   | x==y && even x = MultiSet (S.insert x evens) odds             edges
   | x==y && odd  x = MultiSet evens             (S.insert x odds) edges
   | otherwise      = MultiSet evens              odds            (S.insert p edges)

instance (Integral a, Ord a) => Measured (MultiSet a) (TreeTop a) where
   measure (TreeTop mset _) = mset


delPairMSet :: (Integral a) => (a,a) -> MultiSet a -> MultiSet a
delPairMSet p@(x,y) (MultiSet evens odds edges)
   | x==y && even x = MultiSet (S.delete x evens)            odds             edges
   | x==y && odd  x = MultiSet             evens (S.delete x odds)            edges
   | otherwise      = MultiSet             evens             odds (S.delete p edges)

diffSetMSet :: (Integral a) => MultiSet a -> MultiSet a -> MultiSet a
diffSetMSet (MultiSet evens odds edges) (MultiSet evens' odds' edges') =
   MultiSet (evens S.\\ evens') (odds S.\\ odds') (edges S.\\ edges')

emptyTreeTop ::   Integral a => TreeTop a
emptyTreeTop =    TreeTop emptyMSet empty
emptyForestTop :: Integral a => ForestTop a
emptyForestTop =  ForestTop emptyMSet empty


ftTree   (TreeTop _ Empty) = Empty
ftTree   (TreeTop _ ft   ) = ft
msetTree (TreeTop _ Empty) = emptyMSet
msetTree (TreeTop mset _ ) = mset

ftForest   (ForestTop _ Empty) = Empty
ftForest   (ForestTop _ ft   ) = ft
msetForest (ForestTop _ Empty) = emptyMSet
msetForest (ForestTop mset _ ) = mset

nnodesTree :: TreeTop a -> Int
nnodesTree (TreeTop _ Empty) = 0
nnodesTree (TreeTop (MultiSet evens odds _) _) = S.size evens + S.size odds
sizeTree  :: TreeTop a -> Int
sizeTree  (TreeTop _ Empty) = 0
sizeTree  tree@(TreeTop (MultiSet _ _ edges) _ ) = S.size edges + nnodesTree tree

nnodesForest :: ForestTop a -> Int
nnodesForest (ForestTop _ Empty) = 0
nnodesForest (ForestTop (MultiSet evens odds _) _ ) = S.size evens + S.size odds
sizeForest  :: ForestTop a -> Int
sizeForest  (ForestTop _ Empty) = 0
sizeForest  forest@(ForestTop (MultiSet _ _ edges) _ ) = S.size edges + nnodesForest forest


-- | /O(1)/. Add an element to the left end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(<|) :: (Measured v a) => a -> FingerTree v a -> FingerTree v a
a <| Empty                           =  Single a
a <| Single b                        =  deep (One a) Empty (One b)
a <| Deep _ (Four b c d e) Empty sf  =  Deep mempty (One a)   (deep (One (node2 b c)) Empty (One (node2 d e)) ) sf
a <| Deep _ (Four b c d e) m     sf  =  m `seq` Deep mempty (Two a b) (node3 c d e <| m) sf
a <| Deep v pr m sf              =  Deep mempty (consDigit a pr) m sf

consDigit :: a -> Digit a -> Digit a
consDigit a (One b) = Two a b
consDigit a (Two b c) = Three a b c
consDigit a (Three b c d) = Four a b c d
consDigit _ (Four _ _ _ _) = error "consDigit" -- illegal_argument "consDigit"

-- | /O(1)/. Add an element to the right end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(|>) :: (Measured v a) => FingerTree v a -> a -> FingerTree v a
Empty     |>  a                  =  Single a
Single a  |>  b                  =  deep (One a) Empty (One b)
--Deep v pr m (Four a b c d) ||> e  =  m `seq` Deep mempty pr (m ||> node3 a b c) (Two d e)
Deep v pr Empty (Four a b c d) |> e  =  Deep mempty pr (deep (One (node2 a b)) Empty (One (node2 c d))) (One e)
Deep v pr m (Four a b c d) |> e  =  m `seq` Deep mempty pr (m |> node3 a b c) (Two d e)
Deep v pr m             sf |> x  =  Deep mempty pr m (snocDigit sf x)


snocDigit :: Digit a -> a -> Digit a
snocDigit (One a) b = Two a b
snocDigit (Two a b) c = Three a b c
snocDigit (Three a b c) d = Four a b c d
snocDigit (Four _ _ _ _) _ = error "snocDigit" -- illegal_argument "snocDigit"

-- | /O(1)/. Is this the empty sequence?
null :: FingerTree v a -> Bool
null Empty = True
null _ = False

-- | /O(1)/. Analyse the left end of a sequence.
viewl :: (Measured v a) => FingerTree v a -> ViewL (FingerTree v) a
viewl Empty                     =  EmptyL
viewl (Single x)                =  x :< Empty
viewl (Deep _ (One x) m sf)     =  x :< rotL m sf
viewl (Deep _ pr m sf)          =  lheadDigit pr :< deep (ltailDigit pr) m sf

rotL :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> FingerTree v a
rotL m sf      =   case viewl m of
    EmptyL  ->  digitToTree sf
    a :< m' ->  Deep mempty (nodeToDigit a) m' sf

rotR :: (Measured v a) => Digit a -> FingerTree v (Node v a) -> FingerTree v a
rotR pr m = case viewr m of
    EmptyR  ->  digitToTree pr
    m' :> a ->  Deep mempty pr m' (nodeToDigit a)


lheadDigit :: Digit a -> a
lheadDigit (One a) = a
lheadDigit (Two a _) = a
lheadDigit (Three a _ _) = a
lheadDigit (Four a _ _ _) = a

ltailDigit :: Digit a -> Digit a
ltailDigit (One _) = error "ltailDigit"-- illegal_argument "ltailDigit"
ltailDigit (Two _ b) = One b
ltailDigit (Three _ b c) = Two b c
ltailDigit (Four _ b c d) = Three b c d

-- | /O(1)/. Analyse the right end of a sequence.
viewr :: (Measured v a) => FingerTree v a -> ViewR (FingerTree v) a
viewr Empty                     =  EmptyR
viewr (Single x)                =  Empty :> x
viewr (Deep _ pr m (One x))     =  rotR pr m :> x
viewr (Deep _ pr m sf)          =  deep pr m (rtailDigit sf) :> rheadDigit sf

rheadDigit :: Digit a -> a
rheadDigit (One a) = a
rheadDigit (Two _ b) = b
rheadDigit (Three _ _ c) = c
rheadDigit (Four _ _ _ d) = d

rtailDigit :: Digit a -> Digit a
rtailDigit (One _) = error "rtailDigit"-- illegal_argument "rtailDigit"
rtailDigit (Two a _) = One a
rtailDigit (Three a b _) = Two a b
rtailDigit (Four a b c _) = Three a b c

digitToTree :: (Measured v a) => Digit a -> FingerTree v a
digitToTree (One a)        = Single a
digitToTree (Two a b)      = deep (One a)   Empty (One b)
digitToTree (Three a b c)  = deep (Two a b) Empty (One c)
digitToTree (Four a b c d) = deep (Two a b) Empty (Two c d)


-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
(><) :: (Measured v a) => FingerTree v a -> FingerTree v a -> FingerTree v a
(><) =  appendTree0

appendTree0 :: (Measured v a) => FingerTree v a -> FingerTree v a -> FingerTree v a
appendTree0 Empty xs =    xs
appendTree0 xs Empty =    xs
appendTree0 (Single x) xs =    x <| xs
appendTree0 xs (Single x) =    xs |> x
appendTree0 (Deep _ pr1 m1 sf1) (Deep _ pr2 m2 sf2) =    deep pr1 (addDigits0 m1 sf1 pr2 m2) sf2

addDigits0 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits0 m1 (One a) (One b) m2 =    appendTree1 m1 (node2 a b) m2
addDigits0 m1 (One a) (Two b c) m2 =    appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (One a) (Three b c d) m2 =    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (One a) (Four b c d e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (One c) m2 =    appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (Two a b) (Two c d) m2 =    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Two a b) (Three c d e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (Four c d e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (One d) m2 =    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Three a b c) (Two d e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Three a b c) (Three d e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (Four d e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (One e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Four a b c d) (Two e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Four a b c d) (Three e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (Four e f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2

appendTree1 :: (Measured v a) => FingerTree v a -> a -> FingerTree v a -> FingerTree v a
appendTree1 Empty a xs =    a <| xs
appendTree1 xs a Empty =    xs |> a
appendTree1 (Single x) a xs =    x <| (a <| xs)
appendTree1 xs a (Single x) =    xs |> a |> x
appendTree1 (Deep _ pr1 m1 sf1) a (Deep _ pr2 m2 sf2) =    deep pr1 (addDigits1 m1 sf1 a pr2 m2) sf2

addDigits1 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits1 m1 (One a) b (One c) m2 =    appendTree1 m1 (node3 a b c) m2
addDigits1 m1 (One a) b (Two c d) m2 =    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (One a) b (Three c d e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (One a) b (Four c d e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (One d) m2 =    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (Two a b) c (Two d e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Two a b) c (Three d e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (Four d e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (One e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Three a b c) d (Two e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Three a b c) d (Three e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (Four e f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (One f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Four a b c d) e (Two f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Four a b c d) e (Three f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (Four f g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2

appendTree2 :: (Measured v a) => FingerTree v a -> a -> a -> FingerTree v a -> FingerTree v a
appendTree2 Empty a b xs =    a <| (b <| xs)
appendTree2 xs a b Empty =    xs |> a |> b
appendTree2 (Single x) a b xs =    x <| (a <| (b <| xs))
appendTree2 xs a b (Single x) =    xs |> a |> b |> x
appendTree2 (Deep _ pr1 m1 sf1) a b (Deep _ pr2 m2 sf2) =    deep pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2

addDigits2 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits2 m1 (One a) b c (One d) m2 =    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits2 m1 (One a) b c (Two d e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (One a) b c (Three d e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (One a) b c (Four d e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (One e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (Two a b) c d (Two e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Two a b) c d (Three e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (Four e f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (One f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Three a b c) d e (Two f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Three a b c) d e (Three f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (Four f g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (One g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Four a b c d) e f (Two g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Four a b c d) e f (Three g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2

appendTree3 :: (Measured v a) => FingerTree v a -> a -> a -> a -> FingerTree v a -> FingerTree v a
appendTree3 Empty a b c xs =    a <| (b <| (c <| xs))
appendTree3 xs a b c Empty =    xs |> a |> b |> c
appendTree3 (Single x) a b c xs =    x <| (a <| (b <| (c <| xs)))
appendTree3 xs a b c (Single x) =    xs |> a |> b |> c |> x
appendTree3 (Deep _ pr1 m1 sf1) a b c (Deep _ pr2 m2 sf2) =    deep pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2

addDigits3 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits3 m1 (One a) b c d (One e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits3 m1 (One a) b c d (Two e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (One a) b c d (Three e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (One a) b c d (Four e f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (One f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (Two a b) c d e (Two f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Two a b) c d e (Three f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (Four f g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (One g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Three a b c) d e f (Two g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Three a b c) d e f (Three g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (Four g h i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (One h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Four a b c d) e f g (Two h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

appendTree4 :: (Measured v a) => FingerTree v a -> a -> a -> a -> a -> FingerTree v a -> FingerTree v a
appendTree4 Empty a b c d xs =    a <| (b <| (c <| (d <| xs)))
appendTree4 xs a b c d Empty =    xs |> a |> b |> c |> d
appendTree4 (Single x) a b c d xs =    x <| (a <| (b <| (c <| (d <| xs))))
appendTree4 xs a b c d (Single x) =    xs |> a |> b |> c |> d |> x
appendTree4 (Deep _ pr1 m1 sf1) a b c d (Deep _ pr2 m2 sf2) =    deep pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2

addDigits4 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits4 m1 (One a) b c d e (One f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits4 m1 (One a) b c d e (Two f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (One a) b c d e (Three f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (One a) b c d e (Four f g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (One g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (Two a b) c d e f (Two g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Two a b) c d e f (Three g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (Four g h i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (One h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Three a b c) d e f g (Two h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Three a b c) d e f g (Three h i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (Four h i j k) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (One i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Four a b c d) e f g h (Two i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Four a b c d) e f g h (Three i j k) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2

data SearchResult v a
    = Position (v,FingerTree v a) a (v,FingerTree v a)
    | OnLeft
    | OnRight
    | Nowhere
    deriving (Eq, Ord, Show)

data Split t a = Split t a t deriving Show

data SplitX t a b = SplitX (MultiSet b,t) a (MultiSet b,t) deriving Show

search :: (Integral a, Measured (MultiSet a) b) =>
          (MultiSet a -> Bool) -> MultiSet a -> FingerTree (MultiSet a) b -> SearchResult (MultiSet a) b
search predi mset ftree
  | predi mset        = let SplitX   (sl,l) x (sr,r) = searchTree predi mempty ftree mempty
                        in  Position (sl,l) x (sr,r)
  | otherwise  = Nowhere  -- it can be either OnRight or OnLeft


searchNode :: (Integral a, Measured (MultiSet a) b)
               => (MultiSet a -> Bool) -> MultiSet a -> Node (MultiSet a) b -> MultiSet a
               -> SplitX (Maybe (Digit b)) b a
searchNode p vl (Node2 _ a b) vr
  | p va        = SplitX (mempty,Nothing) a (mb,Just (One b))
  | otherwise   = SplitX (ma,Just (One a)) b (mempty,Nothing)
  where
    ma  = measure a; mb  = measure b
    va  = vl `mappend` ma
    vb  = mb `mappend` vr
searchNode p vl (Node3 _ a b c) vr
  | p va        = SplitX (mempty,Nothing) a (mbc,Just (Two b c))
  | p vab       = SplitX (ma,Just (One a)) b (mc,Just (One c))
  | otherwise   = SplitX (mab,Just (Two a b)) c (mempty,Nothing)
  where
    ma   =  measure a;        mb  = measure b;  mc  = measure c
    mab  =  ma `mappend` mb;  mbc = mb `mappend` mc
    va   =  vl `mappend` ma
    vab  =  va `mappend` mb
    vc   =  mc `mappend` vr
    vbc  =  mb `mappend` vc

searchDigit :: (Integral a, Measured (MultiSet a) b)
               => (MultiSet a -> Bool) -> MultiSet a -> Digit b -> MultiSet a
               -> SplitX (Maybe (Digit b)) b a
searchDigit _ vl (One a) vr = vl `seq` vr `seq` SplitX (mempty,Nothing) a (mempty,Nothing)
searchDigit p vl (Two a b) vr
  | p va        = SplitX (mempty,Nothing)          a  (measure b,(Just (One b)))
  | otherwise   = SplitX (measure a,Just (One a))  b  (mempty,Nothing)
  where
    va      = vl `mappend` measure a
    vb      = measure b `mappend` vr
searchDigit p vl (Three a b c) vr
  | p va        = SplitX (mempty,Nothing)     a (mbc,Just (Two b c))
  | p vab       = SplitX (ma,Just (One a))    b (measure c,Just (One c))
  | otherwise   = SplitX (mab,Just (Two a b)) c (mempty,Nothing)
  where
    ma   = measure a;        mb   = measure b;    mc   = measure c
    mab  = ma `mappend` mb;  mbc  = mb `mappend` mc
    va   = vl `mappend` ma;  vab  = va `mappend` mb
    vbc  = mb `mappend` vc;  vc   = mc `mappend` vr
searchDigit p vl (Four a b c d) vr
  | p va        = SplitX (mempty,Nothing)          a (mbcd,Just (Three b c d))
  | p vab       = SplitX (ma,Just (One a))         b (mcd,Just (Two c d))
  | p vabc      = SplitX (mab,Just (Two a b))      c (md,Just (One d))
  | otherwise   = SplitX (mabc,Just (Three a b c)) d (mempty,Nothing)
  where
    ma   = measure a;    mb   = measure b
    mc   = measure c;    md   = measure d
    mbcd = mb  `mappend` mc `mappend` md
    mabc = ma  `mappend` mb `mappend` mc
    mcd  = mc  `mappend` md;  mab  = ma  `mappend` mb
    va   = vl  `mappend` ma;  vab  = va  `mappend` mb
    vabc = vab `mappend` mc;  vd   = md  `mappend` vr
    vcd  = mc  `mappend` vd;  vbcd = mb  `mappend` vcd


deepL :: (Measured v a) =>
    Maybe (Digit a) -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deepL Nothing m sf      =   rotL m sf
deepL (Just pr) m sf    =   deep pr m sf

deepR :: (Measured v a) =>
    Digit a -> FingerTree v (Node v a) -> Maybe (Digit a) -> FingerTree v a
deepR pr m Nothing      =   rotR pr m
deepR pr m (Just sf)    =   deep pr m sf


illegal_argument :: String -> a
illegal_argument name =
    error $ "Logic error: " ++ name ++ " called with illegal argument"
{-
newtype Elem a = Elem a deriving Show
instance Ord a => Measured (S.Set (a,a)) (Elem a) where
   measure (Elem x) = S.insert (x,x) S.empty
-}

searchTree :: (Integral a, Measured (MultiSet a) b)
               => (MultiSet a -> Bool) -> MultiSet a -> FingerTree (MultiSet a) b -> MultiSet a
               -> SplitX (FingerTree (MultiSet a) b) b a
searchTree _ _ Empty _ = error "searchTree" -- illegal_argument "searchTree"
searchTree _ vl (Single x) vr = SplitX (vl,Empty) x (vr,Empty)
searchTree p vl (Deep _ pr m sf) vr
  | p vlp        =  let  vm   = collectSetsMid m
                         vmsr = vm `mappend` vsr
                         SplitX (sl,l) x (sr,r)     =  searchDigit p vl pr vmsr
                    in   SplitX (sl `mappend` vl,(maybe Empty digitToTree l))
                                x
                                (sr `mappend` vmsr,(deepL r m sf))
  | p vsr        =  let  vm   = collectSetsMid m
                         vlpm = vlp `mappend` vm
                         SplitX (sl,l) x (sr,r)     =  searchDigit p vlpm sf vr
                    in   SplitX (sl `mappend` vlpm,(deepR pr  m  l))
                                x
                                (sr `mappend` vr,(maybe Empty digitToTree r))
  | otherwise   =  let  SplitX (sml,ml) xs (smr,mr) =  searchTree p vlp m vsr
                        SplitX (sl,l)   x  (sr ,r ) =  searchNode p (vlp `mappend` measure ml) xs (measure mr `mappend` vsr)
                   in   SplitX (sml `mappend` sl ,(deepR pr  ml l)) -- `mappend` vlp, (deepR' pr  ml l))
                               x
                               (smr `mappend` sr ,(deepL r mr sf)) -- `mappend` vsr, (deepL' r mr sf))
  where
    vlp    =  vl  `mappend` (measure pr)     --
    vsr    =  (measure sf) `mappend` vr      --
--    vm     =  collectSetsMid m      -- diffSetMSet (diffSetMSet mset mpr) msf
--    vlpm   =  vlp `mappend` vm      --
--    vmsr   =  vm  `mappend` vsr     --

collectSetsMid :: (Integral a, Measured (MultiSet a) b) => (FingerTree (MultiSet a) b) -> MultiSet a
collectSetsMid Empty      = mempty
collectSetsMid (Single x) = measure x
collectSetsMid (Deep set pr mid sf) = (measure pr) `mappend` collectSetsMid mid `mappend` (measure sf)

pt :: (Eq a, Integral a, Show a) => TreeTop a -> IO ()
pt  = prtTree . et2rt . toListTreeTop
toListTreeTop :: Integral a => TreeTop a -> [(a,a)]
toListTreeTop (TreeTop _ Empty) = []
toListTreeTop (TreeTop _ (Single (Leaf x))) = [x]
toListTreeTop (TreeTop mset ft) = case (viewl ft) of
  EmptyL    -> []
  (Leaf x) :< rest -> x : toListTreeTop (TreeTop mset rest)

pf :: (Integral a, Show a) => ForestTop a -> IO ()
pf (ForestTop _ ft) = ( prtForest . ets2rf ) ( forest ft )
 where
      forest g = case viewl g of
        EmptyL   -> []
        x :<  xs -> toListTreeTop x : forest xs

tlf :: (Integral a) => ForestTop a -> [(a,a)]
tlf = toListForestTop
tlt :: (Integral a) => TreeTop a -> [(a,a)]
tlt = toListTreeTop

toListForestTop :: Integral a => ForestTop a -> [(a,a)]
toListForestTop (ForestTop _ Empty) = []
toListForestTop (ForestTop _ (Single x)) = toListTreeTop x
toListForestTop (ForestTop mset ft) = case (viewl ft) of
  EmptyL    -> []
  x :< rest -> toListTreeTop x ++ toListForestTop (ForestTop mset rest)
