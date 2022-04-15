module Comonad.Fix.WFix where

import Comonad.Store
import Comonad.Traced
import Control.Comonad
import qualified Data.List.NonEmpty as NE
import Data.Monoid

-- >>> peek 3 factorialStore
-- 6
factorialStore :: Store Int Int
factorialStore = extend wfix (store go 0)
  where
    -- extend Comonad w => (w a -> b) -> w a -> w b
    -- wfix :: Comonad w => w (w a -> a) -> a
    -- store go 0 :: Store Int (Store Int Int -> Int)

    go :: Int -> (Store Int Int -> Int)
    go 0 _ = 1
    go n w = n * peeks (subtract 1) w

-- >>> trace (Sum 3) factorialTraced
-- 6
factorialTraced :: Traced (Sum Int) Int
factorialTraced = extend wfix (traced go)
  where
    go :: Sum Int -> Traced (Sum Int) Int -> Int
    go (Sum 0) _ = 1
    go (Sum n) t = n * trace (-1) t

factorialStore2 :: Store Int Int
factorialStore2 = extend wfix (Store (\_s -> go) 0)
  where
    go :: Store Int Int -> Int
    go s | pos s == 0 = 1
    go w = pos w * peeks (subtract 1) w

factorialStore3 :: Store Int Int
factorialStore3 = extend wfix (go <$> idStore)
  where
    idStore :: Store Int Int
    idStore = store id 0
    go :: Int -> Store Int Int -> Int
    go 0 _ = 1
    go n w = n * peek (n - 1) w

-- Sum of current and following numbers at each position
sums :: NE.NonEmpty Int -> NE.NonEmpty Int
sums w = extend wfix (go <$> w)
  where
    go :: Int -> NE.NonEmpty Int -> Int
    go n (_ NE.:| []) = n
    go n (_ NE.:| (x : _)) = n + x
