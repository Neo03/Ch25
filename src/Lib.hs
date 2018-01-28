{-# LANGUAGE InstanceSigs #-}

module Lib where
import Data.Monoid


newtype Compose f g a = Compose { getCompose :: f (g a) }
                          deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f1 (Compose fga) = Compose $ (fmap.fmap) f1 fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure x = Compose $ (pure . pure) x

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ ((<*>) <$> f) <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  --foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance  (Traversable f , Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative f1 => (a -> f1 b) -> Compose f g a  -> f1 (Compose f g b)
  --traverse :: Applicative f1 => (a -> f1 b) -> t a  -> f1 (t b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga
