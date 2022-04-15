{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Comonad.Store.Conway where

import Comonad.Store
import Control.Applicative (ZipList (..))
import Control.Comonad
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Monoid (Ap (..), Sum (..))
import qualified Data.Set as S

-- | We represent a coordinate as a product of sums so we can use 'mappend' for easy shifting
type Coord = (Sum Int, Sum Int)

-- | A game of life grid is a store from Coords to Booleans
type Grid = Store Coord Bool

-- | A type containing a slot for each neighbour
data Neighbours a
  = Neighbours
      a a a
      a   a
      a a a
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Given a coordinate, compute all the neighbours of that position.
neighbourLocations :: Coord -> Neighbours Coord
neighbourLocations s =
  mappend s
    <$> Neighbours
      (-1, -1) (0, -1) (1, -1)
      (-1,  0)         (1,  0)
      (-1,  1) (0,  1) (1,  1)

-- | A cell is alive in the next iteration IFF:
-- * The cell is currently ALIVE AND it has 2 living neighbours
-- OR
-- * The cell has exactly 3 living neighbours
-- OTHERWISE the cell is dead in the next iteration
checkCellAlive :: Grid -> Bool
checkCellAlive grid =
  case (currentCellAlive, numLivingNeighbours) of
    (True, 2) -> True
    (_, 3) -> True
    _ -> False
  where
    currentCellAlive :: Bool
    currentCellAlive = extract grid
    neighboursAlive :: Neighbours Bool
    neighboursAlive = experiment neighbourLocations grid
    numLivingNeighbours :: Int
    numLivingNeighbours = length . filter id . toList $ neighboursAlive

-- | Iterate the game of life by one step
step :: Grid -> Grid
step = extend checkCellAlive

-- | The starting state of the grid
startingGrid :: Grid
startingGrid = store checkAlive (0, 0)
  where
    checkAlive :: Coord -> Bool
    checkAlive coord = S.member coord livingCells

    livingCells :: S.Set Coord
    livingCells = S.fromList [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

-----------------------------------------------------------
---- Drawing

drawGrid :: Int -> Grid -> String
drawGrid size g = unlines $ do
  x <- [0 .. size -1]
  return $ do
    y <- [0 .. size -1]
    return . toChar $ peek (Sum x, Sum y) g
  where
    toChar True = '#'
    toChar False = '.'

-- It recomputes every step in all previous iterations for each next iteration
-- You can fix this using Comonad.Representable.Store :)
animateGrid :: Grid -> IO ()
animateGrid grid =
  putStrLn
    . unlines
    . getZipList
    . getAp
    . foldMap Ap
    . intersperse (pure "|")
    . fmap (ZipList . lines . drawGrid 7)
    . take 6
    $ iterate step grid
