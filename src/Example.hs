{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module Example where

import Control.Monad.Reader
import Data.Functor.Base (TreeF (..))
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Tree
import Data.List

--------------------------------------------------
-- Cata (fold)

-- cata :: (Base t a -> a) -> t -> a
-- cata f = c where c = f . fmap c . project

-- cata :: Functor f => (f a -> a) -> Fix f -> a
-- cata f = f . fmap (cata f) . unFix

-- (f a -> a) is called an f-algebra

sum :: [Int] -> Int
sum = cata go
  where
    go Nil = 0
    go (Cons a !r) = a + r

filter :: (a -> Bool) -> [a] -> [a]
filter p = cata go
  where
    go Nil = []
    go (Cons a as)
      | p a = a : as
      | otherwise = as

-- |
-- >>> :m +Data.Tree
-- >>> let tree = Node 0 [Node 1 [], Node 2 [Node 4 [], Node 5 []], Node 3 [Node 6 []]]
-- >>> putStrLn $ pprint tree
pprint :: Tree Int -> String
pprint = flip runReader 0 . cataA go
  where
    go ::
      TreeF Int (Reader Int String) ->
      Reader Int String
    go (NodeF i rss) = do
      -- rss :: [Reader Int String]
      -- ss  :: [String]
      ss <- local (+ 2) $ sequence rss
      indent <- ask
      let s = replicate indent ' ' ++ "* " ++ show i
      pure $ intercalate "\n" (s : ss)

-- para :: Recursive t => (Base t (t, a) -> a) -> t -> a
-- histo :: Recursive t => (Base t (Cofree (Base t) a) -> a) -> t -> a

--------------------------------------------------
-- Ana (unfold)

-- ana g = a where a = embed . fmap a . g

-- ana :: Functor f => (a -> f a) -> a -> Fix f
-- ana f = Fix . fmap (ana f) . f

-- (a -> f a) is called an f-coalgebra

repeat :: a -> Int -> Tree a
repeat a = ana go
  where
    go 0 = NodeF a []
    go n = NodeF a [n-1..0]

---------------------------------------------------
-- Hylo (fold . unfold)

-- hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
-- hylo f g = h where h = f . fmap h . g

-- hylo :: Functor f => (a -> f a) -> (f b -> b) -> a -> b
-- hylo f g = g . fmap (hylo f g) . f

-- Useful when your recursion structure is shaped like a particular
-- recursive datatype, but you're neither consuming nor producing
-- that recursive datatype.
--
-- For example, the recursion structure of quick sort is a binary tree,
-- but its input and output is a list, not a binary tree.

data BinTreeF a b = Tip | Branch b a b
  deriving (Functor)

-- >>> quicksort [5,4,3,2,1]
quicksort :: Ord a => [a] -> [a]
quicksort = hylo merge split where
  split [] = Tip
  split (x:xs) =
    let (l, r) = partition (< x) xs
     in Branch l x r

  merge Tip = []
  merge (Branch l x r) = l ++ [x] ++ r

data BinTreeF' a b = Tip' a | Branch' b b
  deriving (Functor)

mergesort :: Ord a => [a] -> [a]
mergesort = hylo merge split where
  split [x] = Tip' x
  split xs =
    let (l, r) = splitAt (length xs `div` 2) xs
     in Branch' l r

  merge (Tip' a) = [a]
  merge (Branch' xs ys) = merge' xs ys

  -- TODO
  merge' xs [] = xs
  merge' [] ys = ys
  merge' (x:xs) (y:ys)
    | x < y = x : merge' xs (y:ys)
    | otherwise = y : merge' (x:xs) ys
