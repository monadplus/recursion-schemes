module Comonad.Zipper.WaterLevel where

import Comonad.Zipper
import Control.Comonad

{- Trapping Rain Water

Example: [3,0,2,0,4]

x represent walls
. represent trapped water

        x
x . . . x
x . x . x
x . x . x

You can trap at most 7 drain drops.
-}
problem :: Zipper Int
problem = fromList [3, 0, 2, 0, 4]

max0 :: [Int] -> Int
max0 [] = 0
max0 xs = maximum xs

waterAtPosition :: Zipper Int -> Int
waterAtPosition (Zipper toLeft current toRight)
  = max 0 (containingWallHeight - current)
  where
    containingWallHeight = min (max0 toLeft) (max0 toRight)

-- >>> solution problem
-- 7 -- Correct!
solution :: Zipper Int -> Int
solution z = sum (extend waterAtPosition z)
