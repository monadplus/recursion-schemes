{-# LANGUAGE OverloadedLists #-}

module Comonad.Traced.Dependencies where

import Comonad.Traced
import Control.Comonad
import qualified Data.Set as S
import Data.Function ((&))

-- Dependencies
ingredientsOf :: String -> S.Set String
ingredientsOf "string" = S.fromList ["wool"]
ingredientsOf "sticks" = S.fromList ["wood"]
ingredientsOf "bow" = S.fromList ["sticks", "string"]
ingredientsOf "arrows" = S.fromList ["sticks", "feathers", "stone"]
ingredientsOf "quiver" = S.fromList ["arrows", "bow"]
ingredientsOf "torches" = S.fromList ["coal", "sticks"]
ingredientsOf _ = mempty

--- >>> extract recipes
-- fromList []
-- >>> trace ["string", "torches"] recipes
-- fromList ["coal","sticks","wool"]
-- >>>
recipes :: Traced (S.Set String) (S.Set String)
recipes = traced (foldMap ingredientsOf)

-- >>> recipes =>> traces id & trace ["quiver"]
-- fromList ["arrows","bow","feathers","sticks","stone","string"]
-- >>> recipes =>> traces id =>> traces id & trace ["quiver"]
-- fromList ["arrows","bow","feathers","sticks","stone","string","wood","wool"]

-- >>> trace ["bow"] allIngredientsFor
-- fromList ["bow","sticks","string","wood","wool"]
allIngredientsFor :: Traced (S.Set String) (S.Set String)
allIngredientsFor = extend wfix (selectNext <$> listen recipes)
  where
    selectNext ::
      (S.Set String, S.Set String) ->
      Traced (S.Set String) (S.Set String) ->
      S.Set String
    selectNext (requirements, input) t
      | S.null (S.difference requirements input) = input
      | otherwise = trace (S.difference requirements input) t
