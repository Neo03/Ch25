module Bifunctor where

  
import Data.Bifunctor  
{--
  class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> pac -> pbd
    bimap f g = first f . second g

    first  :: (a -> b) -> pac -> pbc
    first f = bimap f id

    second  :: (b -> c) -> pab -> pac
    second = bimap id
--}
data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)
  first f (Deux a b) = Deux (f a) b
  second f (Deux a b) = Deux a (f b)

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)
  first f (Const a) = Const (f a)
  second _ (Const a) = Const a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)
  first f (Drei a b c) = Drei a (f b) c
  second f (Drei a b c) = Drei a  b (f c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)
  first f (SuperDrei a b) = SuperDrei a (f b)
  second _ (SuperDrei a b) = SuperDrei a b

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a 
  first _ (SemiDrei a) = SemiDrei a
  second _ (SemiDrei a) = SemiDrei a 

data Quadriceps a b c d = Quadriceps a b c d 

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadriceps a b c d) = Quadriceps a b (f c) (g d)
  first f (Quadriceps a b c d) = Quadriceps a b (f c) d
  second f (Quadriceps a b c d) = Quadriceps a b c (f d)

data MyEither a b = MyLeft a | MyRight b 

instance Bifunctor (MyEither) where 
  bimap f _ (MyLeft a) = MyLeft (f a)
  bimap _ g (MyRight b) = MyRight (g b)
  first f (MyLeft a) = MyLeft (f a)
  first _ (MyRight b) = MyRight b 
  second _ (MyLeft a) = MyLeft a
  second g (MyRight b) = MyRight (g b)