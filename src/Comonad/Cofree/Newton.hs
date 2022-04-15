{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Comonad.Cofree.Newton where

import Control.Comonad.Cofree
import Data.Functor.Identity
import Data.Maybe

f :: Double -> Double
f x = (x ^ (2 :: Integer)) - 16

f' :: Double -> Double
f' x = 2 * x

steps :: Double -> Double -> Cofree Maybe Double
steps start delta = coiter coalg start
  where
    coalg :: Double -> Maybe Double
    coalg x = case x - (f x / (f' x + 0.1 {- prevent division by 0 -})) of
        next
            | abs (next - x) < delta -> Nothing
            | otherwise -> Just next

-- Calculate infinite newton's method iterations
-- You need to limit depth
steps' ::  Double -> Cofree Identity Double
steps' start = coiter coalg start
  where
    coalg :: Double -> Identity Double
    coalg x = pure $ x - (f x / f' x)

-- >>> last $ take 10000 $ solution $ steps 0 0.00001
-- 4.000000515777824
solution :: Cofree Maybe Double -> [Double]
solution (a :< as) = a : maybe [] solution as
