{-# LANGUAGE InstanceSigs #-}

module Transf where

--import Control.Monad


newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a)} 

newtype MaybeList a = MaybeList {runMaybeList :: [Maybe a]} 

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show) 

newtype IdentityT f a = IdentityT {runIdentityT :: f a} deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f  fa) 

instance Applicative Identity where
    pure = Identity 
    (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)
    (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)      -- fab == f (a -> b) applicative structure of "f"

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
    return = pure 
    
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b

    --(IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f   -- now begins backtrack
    -- runIdentityT :: IdentityT fa -> fa
    -- fmap . runIdentityT :: Functor f => f (IdentityT f1 a) -> f (f1 a) 
    -- IdentityT :: fa -> IdentityT fa

    (IdentityT ma) >>= f = 
        --let aimb = ma >>= f in undefined        -- Couldn't match type ‘m’ with ‘IdentityT m’  *** Ничего существенного
        --                                        -- Попробуем иначе: Т.к. m - это ещё и функтор, то разложим (>>=) на join и fmap        
        -- *************************************  Следующий шаг ************************************************
        --let aimb :: a                         -- "a" может быть только bottom, т.е. любой тип(в любом типе есть bottom)  
        --    aimb = fmap f ma
        --in undefined                          -- Couldn't match expected type ‘a1’ with actual type ‘m (IdentityT m b)’
        --                                      -- т.е aimb :: m (IdentityT m b), но нам нужен тип m(mb),тк. join :: m(ma) -> mb 
        -- ***********************************   Следующий шаг ********************************************
        --  используем runIdentityT :: IdentityT f a -> fa    
        -- fmap runIdentityT :: Functor f => f (IdentityT f1 a) -> f (f1 a).      --Теперь получаем:             
        -- let aimb :: a
        --     aimb = fmap runIdentityT (fmap f ma)
        -- in aimb = undefined                             -- Couldn't match expected type ‘a1’ with actual type ‘m (m b)’
        --                                                 -- а1 - это тип а из aimb :: a
        -- ************************************* Следующий шаг ********************************************
        -- We’ll use join from Control.Monad 
        -- let aimb :: a
        --     aimb = join(fmap runIdentityT (fmap f ma))
        -- in aimb = undefined                              -- Couldn't match expected type ‘a1’ with actual type ‘m b’
        -- *************************************  Почти готово *******************************************
        -- let aimb = join(fmap runIdentityT (fmap f ma))
        -- in aimb = aimb                                    -- Couldn't match type ‘m’ with ‘IdentityT m’ 
        --  ***********************************  Готово  ************************************************** 
        --  Оборачиваем aimb в IdentityT
        --let aimb = join (fmap runIdentityT (fmap f ma))
        --in (IdentityT aimb)                                
        -- ***********************************  Улучшаем ************************************************** 
        --  Functor low: fmap (f . g) = fmap f . fmap g , Поэтому можем записать так:
        -- IdentityT $ join (fmap (runIdentityT . f) ma)
        -- x >>= f = join (fmap f x) , Поэтому можем записать так:
        IdentityT $ ma >>= runIdentityT . f
        