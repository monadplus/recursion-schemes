{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}

module Comonad.Cofree where

import Control.Comonad

-- For every Functor, there exist a Comonad
data Cofree f a = a :< f (Cofree f a)
  deriving (Functor, Foldable)

deriving instance (Show a, forall x. Show x => Show (f x)) => Show (Cofree f a)

instance (Functor f) => Comonad (Cofree f) where
  extract :: Cofree f a -> a
  extract (a :< _) = a
  duplicate :: Cofree f a -> Cofree f (Cofree f a)
  duplicate w@(_ :< rest) = w :< fmap duplicate rest

coiter :: Functor f => (a -> f a) -> a -> Cofree f a
coiter f a = a :< (coiter f <$> f a)

unfold :: Functor f => (b -> (a, f b)) -> b -> Cofree f a
unfold f c = case f c of
  (x, d) -> x :< fmap (unfold f) d

unfoldM :: (Monad m, Traversable f) => (b -> m (a, f b)) -> b -> m (Cofree f a)
unfoldM f seed = do
  (a, next) <- f seed
  fmap (a :<) $ traverse (unfoldM f) next
