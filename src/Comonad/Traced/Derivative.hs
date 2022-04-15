module Comonad.Traced.Derivative where

import Comonad.Traced
import Control.Comonad.Env
import Data.Monoid
import Data.Function ((&))

-- f(x) = x^2 - 16
func :: Sum Double -> Double
func (Sum x) = (x ^ (2 :: Integer)) - 16

f :: Traced (Sum Double) Double
f = traced func

estimateDerivative :: Traced (Sum Double) Double -> Double
estimateDerivative w =
  let leftY = trace (Sum (-1)) w
      rightY = trace (Sum 1) w
   in (rightY - leftY) / 2

-- reader monad
estimateDerivative' :: Traced (Sum Double) Double -> Double
estimateDerivative' = do
  leftY <- trace (Sum (-1))
  rightY <- trace (Sum 1)
  return $ (rightY - leftY) / 2

estimateDerivativeOf ::
  Traced (Sum Double) Double ->
  Traced (Sum Double) Double
estimateDerivativeOf = extend estimateDerivative

-- |
-- >>> trace (Sum 0) withDerivative
-- (-16.0,0.0)
-- >>> trace (Sum 1) withDerivative
-- (-15.0,2.0)
withDerivative :: Traced (Sum Double) (Double, Double)
withDerivative = liftW2 (,) f (estimateDerivativeOf f)

-- |
-- >>> derivativeOf (\x -> x^2 - 16) 4
-- 8
derivativeOf :: (Double -> Double) -> (Double -> Double)
derivativeOf g = runTraced tracedDerivative . Sum
  where
    tracedF = traced (g . getSum)
    tracedDerivative = estimateDerivativeOf tracedF
