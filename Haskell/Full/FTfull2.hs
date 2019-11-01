{-# LANGUAGE CPP, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}

module FTfull where

import Prelude hiding (null)
import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Control.DeepSeq
import Data.Foldable (Foldable(foldMap), toList)
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S

type    SetPair a = S.Set (a,a)

data FingerTree elem
    = Empty
    | Single elem
    | Deep   !(S.Set elem) !(Digit elem) (FingerTree (Node elem)) !(Digit elem) deriving Show

data Digit  elem
    = One   elem
    | Two   elem elem
    | Three elem elem elem
    | Four  elem elem elem elem deriving Show

data Node elem
    = Node2 !(S.Set elem) elem elem
    | Node3 !(S.Set elem) elem elem elem deriving Show

class Edges a where
    edges :: a -> SetPair Int
{-
class Sized a where
     size :: a -> Int

instance Sized a => Sized (FingerTree a) where
        size Empty              = 0
        size (Single x)         = size x
        size (Deep v _ _ _)     = v
-}
instance (Edges elem) => Edges (FingerTree elem) where
    edges Empty            = mempty
    edges (Single x)       = edges x
    edges (Deep set _ _ _) = set

instance (Edges elem) => Edges (Node elem) where
    edges (Node2 set _ _)   = set
    edges (Node3 set _ _ _) = set

instance (Edges elem) => Edges (Digit elem) where
    edges = foldMap edges

instance Foldable Digit where
    foldMap f (One   a)       = f a
    foldMap f (Two   a b)     = f a <> f b
    foldMap f (Three a b c)   = f a <> f b <> f c
    foldMap f (Four  a b c d) = f a <> f b <> f c <> f d


ft1,fte,fts :: FingerTree (Int,Int)
ft1 =  Deep (S.fromList (zip[1..][1..10])) (Two (1,1)(2,2)) Empty (Three (3,3)(4,4)(5,5))
fte = Empty
fts =  Single (1,1)

{-
instance (Show a) => Show (FingerTree a) where
    show Empty             = "[X]"
    show (Single x)        = "["++show x++"]"
    show (Deep _ pr mi sf) = "{"++show pr++";"++show mi++";"++show sf++"}"
-}

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

instance (Functor s) => Functor (ViewL s) where
    fmap _ EmptyL    = EmptyL
    fmap f (x :< xs) = f x :< fmap f xs

instance (Functor s) => Functor (ViewR s) where
    fmap _ EmptyR    = EmptyR
    fmap f (xs :> x) = fmap f xs :> f x

instance (Edges a) => Semigroup (FingerTree a) where
    (<>) = (><)

-- | 'empty' and '><'.
instance (Edges a) => Monoid (FingerTree a) where
    mempty = empty
    mappend = (><)


instance Foldable Node where
    foldMap f (Node2 _ a b)   = f a <> f b
    foldMap f (Node3 _ a b c) = f a <> f b <> f c

node2 :: (Edges a) => a -> a -> Node a
node2 a b = Node2 (edges a <> edges b) a b

node3 :: (Edges a) => a -> a -> a -> Node a
node3 a b c = Node3 (edges a <> edges b <> edges c) a b c

nodeToDigit :: Node a -> Digit a
nodeToDigit (Node2 _ a b) = Two a b
nodeToDigit (Node3 _ a b c) = Three a b c


instance NFData a => NFData (FingerTree a) where
    rnf (Empty) = ()
    rnf (Single x) = rnf x
    rnf (Deep _ pr m sf) = rnf pr `seq` rnf m `seq` rnf sf

instance NFData a => NFData (Digit a) where
    rnf (One a) = rnf a
    rnf (Two a b) = rnf a `seq` rnf b
    rnf (Three a b c) = rnf a `seq` rnf b `seq` rnf c
    rnf (Four a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

instance NFData a => NFData (Node a) where
    rnf (Node2 _ a b) = rnf a `seq` rnf b
    rnf (Node3 _ a b c) = rnf a `seq` rnf b `seq` rnf c

deep :: (Edges a) => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep pr m sf = Deep ((edges pr `mappend`  edges m) <> edges sf) pr m sf

-- | Elements from left to right.
instance Foldable FingerTree  where
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Deep _ pr m sf) =
        foldMap f pr <> foldMap (foldMap f) m <> foldMap f sf

instance (Eq a) => Eq (FingerTree a) where
    xs == ys = toList xs == toList ys

-- | Lexicographical order from left to right.
instance (Ord a) => Ord (FingerTree a) where
    compare xs ys = compare (toList xs) (toList ys)
{-
instance (Show a) => Show (FingerTree v a) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (toList xs)
-}

-----------------------------------------------------
-- 4.3 Construction, deconstruction and concatenation
-----------------------------------------------------
infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

-- | /O(1)/. The empty sequence.
empty :: Edges a => FingerTree a
empty = Empty

-- | /O(1)/. A singleton sequence.
singleton :: Edges a => a -> FingerTree a
singleton = Single

-- | /O(n)/. Create a sequence from a finite list of elements.
-- The opposite operation 'toList' is supplied by the 'Foldable' instance.
fromList :: (Edges a) => [a] -> FingerTree a
fromList = foldr (<|) Empty

-- | /O(1)/. Add an element to the left end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(<|) :: (Edges a) => a -> FingerTree a -> FingerTree a
a <| Empty     =  Single a
a <| Single b  =  deep (One a) Empty (One b)
a <| Deep v (Four b c d e) m sf = m `seq`
    Deep (edges a <> v) (Two a b) (node3 c d e <| m) sf
a <| Deep v pr m sf  =
    Deep (edges a <> v) (consDigit a pr) m sf

consDigit :: a -> Digit a -> Digit a
consDigit a (One b) = Two a b
consDigit a (Two b c) = Three a b c
consDigit a (Three b c d) = Four a b c d
consDigit _ (Four _ _ _ _) = illegal_argument "consDigit"

-- | /O(1)/. Add an element to the right end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(|>) :: (Edges a) => FingerTree a -> a -> FingerTree a
Empty    |> a  =  Single a
Single a |> b  =  deep (One a) Empty (One b)
Deep v pr m (Four a b c d) |> e = m `seq`
    Deep (v <> edges e) pr (m |> node3 a b c) (Two d e)
Deep v pr m sf |> x  =
    Deep (v <> edges x) pr m (snocDigit sf x)

snocDigit :: Digit a -> a -> Digit a
snocDigit (One a)        b = Two   a b
snocDigit (Two a b)      c = Three a b c
snocDigit (Three a b c)  d = Four  a b c d
snocDigit (Four _ _ _ _) _ = illegal_argument "snocDigit"

-- | /O(1)/. Is this the empty sequence?
null ::                   FingerTree a -> Bool
null Empty = True
null _     = False

-- | /O(1)/. Analyse the left end of a sequence.
viewl :: (Edges a) => FingerTree a -> ViewL (FingerTree ) a
viewl Empty                 =  EmptyL
viewl (Single x)            =  x :< Empty
viewl (Deep _ (One x) m sf) =  x :< rotL m sf
viewl (Deep _ pr m sf)      =  lheadDigit pr :< deep (ltailDigit pr) m sf

rotL :: (Edges a) => FingerTree (Node a) -> Digit a -> FingerTree a
rotL m sf = case viewl m of
    EmptyL  -> digitToTree sf
    a :< m' -> Deep (edges m <> edges sf) (nodeToDigit a) m' sf

lheadDigit :: Digit a -> a
lheadDigit (One a)        = a
lheadDigit (Two a _)      = a
lheadDigit (Three a _ _)  = a
lheadDigit (Four a _ _ _) = a

ltailDigit :: Digit a -> Digit a
ltailDigit (One _)        = illegal_argument "ltailDigit"
ltailDigit (Two _ b)      = One   b
ltailDigit (Three _ b c)  = Two   b c
ltailDigit (Four _ b c d) = Three b c d

-- | /O(1)/. Analyse the right end of a sequence.
viewr :: (Edges a) => FingerTree a -> ViewR (FingerTree ) a
viewr Empty                 =  EmptyR
viewr (Single x)            =  Empty :> x
viewr (Deep _ pr m (One x)) =  rotR pr m :> x
viewr (Deep _ pr m sf)      =  deep pr m (rtailDigit sf) :> rheadDigit sf

rotR :: (Edges a) => Digit a -> FingerTree (Node a) -> FingerTree a
rotR pr m = case viewr m of
    EmptyR  -> digitToTree pr
    m' :> a -> Deep (edges pr `mappend` edges m) pr m' (nodeToDigit a)

rheadDigit :: Digit a -> a
rheadDigit (One   a)       = a
rheadDigit (Two   _ b)     = b
rheadDigit (Three _ _ c)   = c
rheadDigit (Four  _ _ _ d) = d

rtailDigit :: Digit a -> Digit a
rtailDigit (One   _)       = illegal_argument "rtailDigit"
rtailDigit (Two   a _)     = One   a
rtailDigit (Three a b _)   = Two   a b
rtailDigit (Four  a b c _) = Three a b c

digitToTree :: (Edges a) => Digit a -> FingerTree a
digitToTree (One   a)       = Single a
digitToTree (Two   a b)     = deep (One a)   Empty (One b)
digitToTree (Three a b c)   = deep (Two a b) Empty (One c)
digitToTree (Four  a b c d) = deep (Two a b) Empty (Two c d)

----------------
-- Concatenation
----------------

-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
(><) :: (Edges a) => FingerTree a -> FingerTree a -> FingerTree a
(><) = appendTree0

appendTree0 :: (Edges a) => FingerTree a -> FingerTree a -> FingerTree a
appendTree0 Empty               xs                   =  xs
appendTree0 xs                  Empty                =  xs
appendTree0 (Single x)          xs                   =  x <| xs
appendTree0 xs                  (Single x)           =  xs |> x
appendTree0 (Deep _ pr1 m1 sf1) (Deep _ pr2 m2 sf2)  =  deep pr1 (addDigits0 m1 sf1 pr2 m2) sf2

addDigits0 :: (Edges a) => FingerTree (Node a) -> Digit a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
addDigits0 m1 (One a)        (One b)        m2 = appendTree1 m1 (node2 a b)   m2
addDigits0 m1 (One a)        (Two b c)      m2 = appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (One a)        (Three b c d)  m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits0 m1 (One a)        (Four b c d e) m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits0 m1 (Two a b)      (One c)        m2 = appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (Two a b)      (Two c d)      m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits0 m1 (Two a b)      (Three c d e)  m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits0 m1 (Two a b)      (Four c d e f) m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c)  (One d)        m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits0 m1 (Three a b c)  (Two d e)      m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits0 m1 (Three a b c)  (Three d e f)  m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c)  (Four d e f g) m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g) m2
addDigits0 m1 (Four a b c d) (One e)        m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits0 m1 (Four a b c d) (Two e f)      m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Four a b c d) (Three e f g)  m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g) m2
addDigits0 m1 (Four a b c d) (Four e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2

appendTree1 :: (Edges a) => FingerTree a -> a -> FingerTree a -> FingerTree a
appendTree1 Empty               a xs                  = a <| xs
appendTree1 xs                  a Empty               = xs |> a
appendTree1 (Single x)          a xs                  = x <| a <| xs
appendTree1 xs                  a (Single x)          = xs |> a |> x
appendTree1 (Deep _ pr1 m1 sf1) a (Deep _ pr2 m2 sf2) = deep pr1 (addDigits1 m1 sf1 a pr2 m2) sf2

addDigits1 :: (Edges a) => FingerTree (Node a) -> Digit a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
addDigits1 m1 (One a)        b (One c)        m2 = appendTree1 m1 (node3 a b c) m2
addDigits1 m1 (One a)        b (Two c d)      m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits1 m1 (One a)        b (Three c d e)  m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits1 m1 (One a)        b (Four c d e f) m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b)      c (One d)        m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits1 m1 (Two a b)      c (Two d e)      m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits1 m1 (Two a b)      c (Three d e f)  m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b)      c (Four d e f g) m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits1 m1 (Three a b c)  d (One e)        m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits1 m1 (Three a b c)  d (Two e f)      m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Three a b c)  d (Three e f g)  m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits1 m1 (Three a b c)  d (Four e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits1 m1 (Four a b c d) e (One f)        m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Four a b c d) e (Two f g)      m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits1 m1 (Four a b c d) e (Three f g h)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits1 m1 (Four a b c d) e (Four f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2

appendTree2 :: (Edges a) => FingerTree a -> a -> a -> FingerTree a -> FingerTree a
appendTree2 Empty               a b xs                  = a <| b <| xs
appendTree2 xs                  a b Empty               = xs |> a |> b
appendTree2 (Single x)          a b xs                  = x <| a <| b <| xs
appendTree2 xs                  a b (Single x)          = xs |> a |> b |> x
appendTree2 (Deep _ pr1 m1 sf1) a b (Deep _ pr2 m2 sf2) = deep pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2

addDigits2 :: (Edges a) => FingerTree (Node a) -> Digit a -> a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
addDigits2 m1 (One a)        b c (One d)        m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits2 m1 (One a)        b c (Two d e)      m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits2 m1 (One a)        b c (Three d e f)  m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (One a)        b c (Four d e f g) m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits2 m1 (Two a b)      c d (One e)        m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits2 m1 (Two a b)      c d (Two e f)      m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Two a b)      c d (Three e f g)  m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits2 m1 (Two a b)      c d (Four e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits2 m1 (Three a b c)  d e (One f)        m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Three a b c)  d e (Two f g)      m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits2 m1 (Three a b c)  d e (Three f g h)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits2 m1 (Three a b c)  d e (Four f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (One g)        m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits2 m1 (Four a b c d) e f (Two g h)      m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits2 m1 (Four a b c d) e f (Three g h i)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j) m2

appendTree3 :: (Edges a) => FingerTree a -> a -> a -> a -> FingerTree a -> FingerTree a
appendTree3 Empty               a b c xs                  = a <| b <| c <| xs
appendTree3 xs                  a b c Empty               = xs |> a |> b |> c
appendTree3 (Single x)          a b c xs                  = x <| a <| b <| c <| xs
appendTree3 xs                  a b c (Single x)          = xs |> a |> b |> c |> x
appendTree3 (Deep _ pr1 m1 sf1) a b c (Deep _ pr2 m2 sf2) = deep pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2

addDigits3 :: (Edges a) => FingerTree (Node a) -> Digit a -> a -> a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
addDigits3 m1 (One a)        b c d (One e)        m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits3 m1 (One a)        b c d (Two e f)      m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (One a)        b c d (Three e f g)  m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits3 m1 (One a)        b c d (Four e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits3 m1 (Two a b)      c d e (One f)        m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (Two a b)      c d e (Two f g)      m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits3 m1 (Two a b)      c d e (Three f g h)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits3 m1 (Two a b)      c d e (Four f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c)  d e f (One g)        m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits3 m1 (Three a b c)  d e f (Two g h)      m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits3 m1 (Three a b c)  d e f (Three g h i)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c)  d e f (Four g h i j) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (One h)        m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits3 m1 (Four a b c d) e f g (Two h i)      m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j)  m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

appendTree4 :: (Edges a) => FingerTree a -> a -> a -> a -> a -> FingerTree a -> FingerTree a
appendTree4 Empty               a b c d xs                  = a <| b <| c <| d <| xs
appendTree4 xs                  a b c d Empty               = xs |> a |> b |> c |> d
appendTree4 (Single x)          a b c d xs                  = x <| a <| b <| c <| d <| xs
appendTree4 xs                  a b c d (Single x)          = xs |> a |> b |> c |> d |> x
appendTree4 (Deep _ pr1 m1 sf1) a b c d (Deep _ pr2 m2 sf2) = deep pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2

addDigits4 :: (Edges a) => FingerTree (Node a) -> Digit a -> a -> a -> a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
addDigits4 m1 (One a)        b c d e (One f)        m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits4 m1 (One a)        b c d e (Two f g)      m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits4 m1 (One a)        b c d e (Three f g h)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits4 m1 (One a)        b c d e (Four f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b)      c d e f (One g)        m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits4 m1 (Two a b)      c d e f (Two g h)      m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits4 m1 (Two a b)      c d e f (Three g h i)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b)      c d e f (Four g h i j) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j)   m2
addDigits4 m1 (Three a b c)  d e f g (One h)        m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits4 m1 (Three a b c)  d e f g (Two h i)      m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Three a b c)  d e f g (Three h i j)  m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j)   m2
addDigits4 m1 (Three a b c)  d e f g (Four h i j k) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k)   m2
addDigits4 m1 (Four a b c d) e f g h (One i)        m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Four a b c d) e f g h (Two i j)      m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j)   m2
addDigits4 m1 (Four a b c d) e f g h (Three i j k)  m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k)   m2
addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2

----------------
-- 4.4 Splitting
----------------

-- | /O(log(min(i,n-i)))/. Split a sequence at a point where the predicate
-- on the accumulated edges of the prefix changes from 'False' to 'True'.
--
-- For predictable results, one should ensure that there is only one such
-- point, i.e. that the predicate is /monotonic/.
split :: (Edges a) =>         (v -> Bool) -> FingerTree a -> (FingerTree a, FingerTree a)
split _ Empty = (Empty, Empty)
split p xs
  | p (edges xs) = (l, x <| r)
  | otherwise      = (xs, Empty)
  where
    Split l x r = splitTree p mempty xs

data Split t a = Split t a t deriving Show

splitTree :: (Edges a) =>         (v -> Bool) ->     v     -> FingerTree a -> Split (FingerTree a) a
splitTree _ _ Empty = illegal_argument "splitTree"
splitTree _ _ (Single x) = Split Empty x Empty
splitTree p i (Deep _ pr m sf)
  | p vpr     = let Split l x r    = splitDigit p i pr
                in  Split (maybe Empty digitToTree l) x (deepL r m sf)
  | p vm      = let Split ml xs mr = splitTree p vpr m
                    Split l x r    = splitNode p (vpr `mappend` edges ml) xs
                in  Split (deepR pr  ml l) x (deepL r mr sf)
  | otherwise = let Split l x r    = splitDigit p vm sf
                in  Split (deepR pr  m  l) x (maybe Empty digitToTree r)
  where
    vpr = i   <>           edges pr
    vm  = vpr <>           edges m

deepL :: (Edges a) => Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL Nothing m sf   = rotL m sf
deepL (Just pr) m sf = deep pr m sf

deepR :: (Edges a) => Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
deepR pr m Nothing   = rotR pr m
deepR pr m (Just sf) = deep pr m sf

splitNode :: (Edges a) =>         (v -> Bool) ->     v     -> Node a -> Split (Maybe (Digit a)) a
splitNode p i (Node2 _ a b)
  | p va      = Split Nothing a (Just (One b))
  | otherwise = Split (Just (One a)) b Nothing
  where
    va = i <> edges a
splitNode p i (Node3 _ a b c)
  | p va      = Split Nothing a (Just (Two b c))
  | p vab     = Split (Just (One a)) b (Just (One c))
  | otherwise = Split (Just (Two a b)) c Nothing
  where
    va  = i  <> edges a
    vab = va <> edges b

splitDigit :: (Edges a) =>         (v -> Bool) ->     v     -> Digit a -> Split (Maybe (Digit a)) a
splitDigit _ i (One a) = i `seq` Split Nothing a Nothing
splitDigit p i (Two a b)
  | p va      = Split Nothing a (Just (One b))
  | otherwise = Split (Just (One a)) b Nothing
  where
    va = i <> edges a
splitDigit p i (Three a b c)
  | p va      = Split Nothing a (Just (Two b c))
  | p vab     = Split (Just (One a)) b (Just (One c))
  | otherwise = Split (Just (Two a b)) c Nothing
  where
    va  = i  <> edges a
    vab = va <> edges b
splitDigit p i (Four a b c d)
  | p va      = Split Nothing a (Just (Three b c d))
  | p vab     = Split (Just (One a)) b (Just (Two c d))
  | p vabc    = Split (Just (Two a b)) c (Just (One d))
  | otherwise = Split (Just (Three a b c)) d Nothing
  where
    va   = i   <> edges a
    vab  = va  <> edges b
    vabc = vab <> edges c

illegal_argument :: String -> a
illegal_argument name =
    error $ "Logic error: " ++ name ++ " called with illegal argument"


data SearchResult a
    = Position (FingerTree a) a (FingerTree a)
    | OnLeft
    | OnRight
    | Nowhere
    deriving (Eq, Ord, Show)

search :: (Edges a) => (        v ->         v -> Bool) -> FingerTree a -> SearchResult a
search p t
  | p_left     && p_right = OnLeft
  | not p_left && p_right = case searchTree p mempty t mempty of
        Split l x r -> Position l x r
  | not p_left && not p_right = OnRight
  | otherwise = Nowhere
  where
    p_left  = p mempty vt
    p_right = p vt mempty
    vt      = edges t

searchTree :: (Edges a) => (         v ->          v -> Bool) ->         v -> FingerTree a ->          v -> Split (FingerTree a) a
searchTree _ _ Empty _ = illegal_argument "searchTree"
searchTree _ _ (Single x) _ = Split Empty x Empty
searchTree p vl (Deep _ pr m sf) vr
  | p vlp vmsr  =  let  Split l x r     =  searchDigit p vl pr vmsr
                   in   Split (maybe Empty digitToTree l) x (deepL r m sf)
  | p vlpm vsr  =  let  Split ml xs mr  =  searchTree p vlp m vsr
                        Split l x r     =  searchNode p (vlp `mappend` edges ml) xs (edges mr `mappend` vsr)
                   in   Split (deepR pr  ml l) x (deepL r mr sf)
  | otherwise   =  let  Split l x r     =  searchDigit p vlpm sf vr
                   in   Split (deepR pr  m  l) x (maybe Empty digitToTree r)
  where
    vlp     =  vl `mappend` edges pr
    vlpm    =  vlp `mappend` vm
    vmsr    =  vm `mappend` vsr
    vsr     =  edges sf `mappend` vr
    vm      =  edges m


searchNode :: (Edges a) => (        v ->         v -> Bool) ->         v -> Node a ->         v -> Split (Maybe (Digit a)) a
searchNode p vl (Node2 _ a b) vr
  | p va vb     = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    va      = vl `mappend` edges a
    vb      = edges b `mappend` vr
searchNode p vl (Node3 _ a b c) vr
  | p va vbc    = Split Nothing a (Just (Two b c))
  | p vab vc    = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    va      = vl `mappend` edges a
    vab     = va `mappend` edges b
    vc      = edges c `mappend` vr
    vbc     = edges b `mappend` vc

searchDigit :: (Edges a) => (        v ->         v -> Bool) ->         v -> Digit a ->         v -> Split (Maybe (Digit a)) a
searchDigit _ vl (One a) vr = vl `seq` vr `seq` Split Nothing a Nothing
searchDigit p vl (Two a b) vr
  | p va vb     = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    va      = vl `mappend` edges a
    vb      = edges b `mappend` vr
searchDigit p vl (Three a b c) vr
  | p va vbc    = Split Nothing a (Just (Two b c))
  | p vab vc    = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    va      = vl `mappend` edges a
    vab     = va `mappend` edges b
    vbc     = edges b `mappend` vc
    vc      = edges c `mappend` vr
searchDigit p vl (Four a b c d) vr
  | p va vbcd   = Split Nothing a (Just (Three b c d))
  | p vab vcd   = Split (Just (One a)) b (Just (Two c d))
  | p vabc vd   = Split (Just (Two a b)) c (Just (One d))
  | otherwise   = Split (Just (Three a b c)) d Nothing
  where
    va      = vl `mappend` edges a
    vab     = va `mappend` edges b
    vabc    = vab `mappend` edges c
    vbcd    = edges b `mappend` vcd
    vcd     = edges c `mappend` vd
    vd      = edges d `mappend` vr
