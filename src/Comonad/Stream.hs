{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Comonad.Stream where

import Control.Comonad
import Data.Coerce (coerce)
import Data.Foldable
import Data.List (intercalate)

data Stream a = a :> Stream a
  deriving (Functor, Foldable)

instance (Show a) => Show (Stream a) where
  show s = intercalate " :> " (show <$> takeS 5 s) ++ " :> ..."

-----------

instance Comonad Stream where
  extract :: Stream a -> a
  extract (a :> _) = a
  duplicate :: Stream a -> Stream (Stream a)
  duplicate s@(_ :> rest) = s :> duplicate rest
  extend :: (Stream a -> b) -> Stream a -> Stream b
  extend f s@(_ :> rest) = f s :> extend f rest

{- Comonad Laws

1. extend extract      = id
2. extract . extend f  = f
3. extend f . extend g = extend (f . extend g)

Notice, if Stream didn't have this shape, extend extract would preserve law 1.
-}

-----------

cycleS :: a -> Stream a
cycleS a = a :> cycleS a

fromList :: [a] -> Stream a
fromList xs = go (cycle xs)
  where
    go [] = error "empty list"
    go (a : rest) = a :> go rest

countStream :: Stream Int
countStream = fromList [0 ..]

-----------

ix :: Int -> Stream a -> a
ix n _ | n < 0 = error "position must be positive"
ix 0 (a :> _) = a
ix n (_ :> rest) = ix (n - 1) rest

ix' :: Int -> Stream a -> a
ix' n = extract . dropS n

dropS :: Int -> Stream a -> Stream a
dropS n = extend (ix n)

dropS' :: Int -> Stream a -> Stream a
dropS' n = ix n . duplicate

takeS :: Int -> Stream a -> [a]
takeS n = take n . toList

-- |
-- >>> windows 3 countStream
-- [0,1,2] :> [1,2,3] :> [2,3,4] :> [3,4,5] :> [4,5,6] :> ...
windows :: Int -> Stream a -> Stream [a]
windows n = extend (takeS n)

-- |
-- >>> rollingAvg 5 countStream
-- 2.0 :> 3.0 :> 4.0 :> 5.0 :> 6.0 :> ...
rollingAvg :: Int -> Stream Int -> Stream Double
rollingAvg windowSize = extend (windowedAvg windowSize)

windowedAvg :: Int -> Stream Int -> Double
windowedAvg windowSize s = avg (takeS windowSize s)

avg :: [Int] -> Double
avg xs =
  fromIntegral (sum xs)
    / fromIntegral (length xs)

{- Extend operator

(=>>) :: w a -> (w a -> b) -> w b

>>> countStream =>> ix 2 =>> ix 2
4 :> 5 :> 6 :> 7 :> 8 :> ...
>>> countStream =>> (ix 2 . extend (ix 2))
4 :> 5 :> 6 :> 7 :> 8 :> ...

^^^ This corresponds to law 3.
-}


{- Cokleisli composition

newtype Cokleisli w a b = Cokleisli { runCokleisli :: w a -> b }

(=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)

>>> countStream =>> (ix 2 =>= ix 2)
4 :> 5 :> 6 :> 7 :> 8 :> ...
-}
