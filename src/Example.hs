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
import Data.List
import Data.Tree

--------------------------------------------------
-- Cata (fold)

-- cata :: (Base t a -> a) -> t -> a
-- cata f = c where c = f . fmap c . project

-- cata :: Functor f => (f a -> a) -> Fix f -> a
-- cata f = f . fmap (cata f) . unFix

-- (f a -> a) is called an f-algebra

sum' :: [Int] -> Int
sum' = cata go
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
-- * 0
--   * 1
--   * 2
--     * 4
--     * 5
--   * 3
--     * 6
pprint :: Tree Int -> String
pprint = flip runReader 0 . cataA go
  where
    go ::
      TreeF Int (Reader Int String) ->
      Reader Int String
    go (NodeF i rss) = do
      indent <- ask -- 1* Order does not matter thanks to `local`
      ss <- local (+ 2) $ sequence rss
      -- 1* indent <- ask
      let s = replicate indent ' ' ++ "* " ++ show i
      pure $ intercalate "\n" (s : ss)

-- para :: Recursive t => (Base t (t, a) -> a) -> t -> a

-- |
-- >>> :m +Data.Tree
-- >>> let tree = Node 0 [Node 1 [], Node 2 [Node 4 [], Node 5 []], Node 3 [Node 6 []]]
-- >>> putStrLn $ pprint2 tree
-- * 0 (6)
--   * 1 (0)
--   * 2 (2)
--     * 4 (0)
--     * 5 (0)
--   * 3 (1)
--     * 6 (0)
pprint2 :: Tree Int -> String
pprint2 = flip runReader 0 . para go
  where
    go ::
      TreeF Int (Tree Int, Reader Int String) ->
      Reader Int String
    go (NodeF i trss) = do
      let (ts, rss) = unzip trss
      let count = sum $ fmap length ts
      ss <- local (+ 2) $ sequence rss
      indent <- ask
      let s =
            replicate indent ' '
              ++ "* "
              ++ show i
              ++ " ("
              ++ show count
              ++ ")"
      pure $ intercalate "\n" (s : ss)

-- histo :: Recursive t => (Base t (Cofree (Base t) a) -> a) -> t -> a
-- Example: https://hackage.haskell.org/package/recursion-schemes-5.2.2.2/docs/Data-Functor-Foldable.html#v:histo

-- zygo :: Recursive t => (Base t b -> b) -> (Base t (b, a) -> a) -> t -> a

--------------------------------------------------
-- Ana (unfold)

-- ana :: Corecursive t => (a -> Base t a) -> a -> t
-- ana g = a where a = embed . fmap a . g

-- ana :: Functor f => (a -> f a) -> a -> Fix f
-- ana f = Fix . fmap (ana f) . f

-- (a -> f a) is called an f-coalgebra

-- |
-- >>> let tree = Example.repeat 0 3
-- >>> putStrLn $ pprint2 tree
-- * 1 (7)
--   * 1 (3)
--     * 1 (1)
--       * 1 (0)
--     * 1 (0)
--   * 1 (1)
--     * 1 (0)
--   * 1 (0)
repeat :: a -> Int -> Tree a
repeat a = ana go
  where
    go 0 = NodeF a []
    go n = NodeF a [n -1, n -2 .. 0]

-- apo :: Corecursive t => (a -> Base t (Either t a)) -> a -> t

-- |
-- >>> let tree = Example.repeat2 0 2
-- >>> putStrLn $ pprint2 tree
-- * 0 (13)
--   * 0 (0)
--   * 0 (4)
--     * 0 (0)
--     * 0 (1)
--       * 0 (0)
--     * 0 (0)
--   * 0 (3)
--     * 0 (1)
--       * 0 (0)
--     * 0 (0)
--   * 0 (1)
--     * 0 (0)
--   * 0 (0)
repeat2 :: a -> Int -> Tree a
repeat2 a = apo go
  where
    go 0 = NodeF a []
    go n
      | odd n = NodeF a (Left (Node a []) : fmap Right [n -2, n -3 .. 0])
      | otherwise = NodeF a (fmap Right [n -1, n -2 .. 0])

-- futu :: Corecursive t => (a -> Base t (Free (Base t) a)) -> a -> t

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

-- |
-- >>> quicksort [5,4,3,2,1]
quicksort :: Ord a => [a] -> [a]
quicksort = hylo merge split
  where
    split [] = Tip
    split (x : xs) =
      let (l, r) = partition (< x) xs
       in Branch l x r

    merge Tip = []
    merge (Branch l x r) = l ++ [x] ++ r

data BinTreeF' a b = Tip' a | Branch' b b
  deriving (Functor)

-- |
-- >>> mergesort [1,2,3,4,5]
-- [1,2,3,4,5]
-- >>> mergesort [5,4,3,2,1]
-- [1,2,3,4,5]
-- >>> mergesort [1,4,2,5,3]
-- [1,2,3,4,5]
mergesort :: Ord a => [a] -> [a]
mergesort = \case
  [] -> []
  xs -> hylo merge split xs
  where
    split [x] = Tip' x
    split xs =
      let (l, r) = splitAt (length xs `div` 2) xs
       in Branch' l r

    merge (Tip' x) = [x]
    merge (Branch' xs ys) = merge' xs ys
      where
        merge' xs [] = xs
        merge' [] ys = ys
        merge' (x : xs) (y : ys)
          | x < y = x : merge' xs (y : ys)
          | otherwise = y : merge' (x : xs) ys
