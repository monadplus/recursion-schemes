module Comonad.Store.Convolution where

import Comonad.Store
import Control.Applicative
import Control.Comonad
import Data.Maybe
import Data.Monoid

(!?) :: [a] -> Int -> Maybe a
xs !? i
  | i < 0 || i >= length xs = Nothing
  | otherwise = Just (xs !! i)

mkStore :: (Num a) => [[a]] -> Store (Sum Int, Sum Int) a
mkStore img = store (fromMaybe 0 . get) (0, 0)
  where
    get (Sum x, Sum y) = do
      row <- img !? fromIntegral x
      row !? fromIntegral y

imageStore :: Store (Sum Int, Sum Int) Double
imageStore = mkStore image
  where
    image :: [[Double]]
    image =
      [ [3, 5, 8],
        [4, 5, 9],
        [2, 1, 7]
      ]

drawImage :: (Show a) => Int -> Store (Sum Int, Sum Int) a -> String
drawImage size g = unlines $ do
  x <- [0 .. size -1]
  let ls = do
        y <- [0 .. size -1]
        show (peek (Sum x, Sum y) g) <> " "
  return ls

printImage :: (Show a) => Int -> Store (Sum Int, Sum Int) a -> IO ()
printImage size g = putStrLn $ drawImage size g

neighbours :: Num i => (i, i) -> [(i, i)]
neighbours (x, y) = liftA2 (,) [x - 1, x, x + 1] [y - 1, y, y + 1]

gauss1 :: Store (Sum Int, Sum Int) Double -> Double
gauss1 w =
  let context = experiment neighbours w
   in sum context / fromIntegral (length context)

-- |
-- >>> printImage 3 $ gauss imageStore
-- 1.8888888888888888 3.7777777777777777 3.0
-- 2.2222222222222223 4.888888888888889 3.888888888888889
-- 1.3333333333333333 3.111111111111111 2.4444444444444446
gauss :: Store (Sum Int, Sum Int) Double -> Store (Sum Int, Sum Int) Double
gauss = extend gauss1
