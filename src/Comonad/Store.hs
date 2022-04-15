{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Comonad.Store where

import Control.Comonad
import qualified Data.Map as M

-- | Co-state
data Store s a = Store (s -> a) s
  deriving (Functor, ComonadApply)

store :: (s -> a) -> s -> Store s a
store = Store

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) =
    Store (\s' -> Store f s') s

instance Monoid s => Applicative (Store s) where
  pure a = Store (const a) mempty
  Store a i <*> Store b j = Store (a <*> b) (i <> j)

-- Get focus
pos :: Store s a -> s
pos (Store _ s) = s

-- Peek value associated with s
peek :: s -> Store s a -> a
peek s (Store f _) = f s

-- (Relatively) peek value associated with `f s`
-- For example `peeks (+1) w` peek from current focus + 1
peeks :: (s -> s) -> Store s a -> a
peeks g (Store f s) = f (g s)

-- Move focus
seek :: s -> Store s a -> Store s a
seek s (Store f _) = Store f s

-- (Relatively) move focus
seeks :: (s -> s) -> Store s a -> Store s a
seeks g (Store f s) = Store f (g s)

-- See squared example below
experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment search (Store f s) = f <$> search s

------------------------------------------
-- Example on Map

populations :: M.Map String Int
populations =
  M.fromList
    [ ("Canada", 37279811),
      ("Poland", 38028278),
      ("France", 65480710),
      ("United States", 329093110),
      ("Germany", 82438639)
    ]

countryPopulation :: Store String (Maybe Int)
countryPopulation = store (\country -> M.lookup country populations) "Canada"

-- > λ> pos countryPopulation
-- > "Canada"
-- > λ> peek "Poland" countryPopulation
-- > Just 38028278
-- > λ> pos $ seek "Germany" countryPopulation
-- > "Germany"

------------------------------------------
-- More abstract uses of Store

squared :: Store Int Int
squared = Store (^ 2) 10

-- > λ> pos squared
-- > 10
-- > λ> extract squared
-- > 100
-- > λ> peek 2 squared
-- > 4
-- > λ> extract $ seeks (+1) squared
-- > 121
-- > λ> experiment (\n -> [n + 10, n + 20, n + 30]) squared
-- > [400,900,1600]

-- |
-- >>> extract withN
-- "10" 100
withN :: Store Int (String, Int)
withN = squared =>> experiment (\n -> (show n, n))

-- |
-- >>> peek 2 shifted
-- "12" 144
shifted :: Store Int (String, Int)
shifted = withN =>> peeks (+10)

