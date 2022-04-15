{-# LANGUAGE DeriveTraversable #-}

module Comonad.Tree where

import Control.Comonad
import Data.Semigroup
import Control.Arrow((&&&))

data Tree a = Tree
  { root :: a,
    children :: [Tree a]
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Comonad Tree where
  extract (Tree root _) = root
  duplicate w@(Tree _ children) = Tree w (duplicate <$> children)
  {-# INLINE extract #-}

data SkillNode = SN
  { skillName :: String,
    pointsSpent :: Int,
    pointsAvailable :: Int
  }
  deriving (Eq)

instance Show SkillNode where
  show sn = skillName sn <> " (" <> show (pointsSpent sn) <> "/" <> show (pointsAvailable sn) <> ")"

getPointsLeft :: SkillNode -> Int
getPointsLeft sn = pointsAvailable sn - pointsSpent sn

magic, fireball, flamewall, levitation :: SkillNode
magic = SN "Magic" 2 5
fireball = SN "Fireball" 1 3
flamewall = SN "Flamewall" 0 1
levitation = SN "Levitation" 1 2

sampleTree :: Tree SkillNode
sampleTree =
  Tree
    magic
    [ Tree fireball [Tree flamewall []],
      Tree levitation []
    ]

countRemainingPoints :: Tree SkillNode -> Int
countRemainingPoints t = getSum $ foldMap (Sum . getPointsLeft) t

-- |
-- >>> annotateNodes sampleTree
-- Tree {root = (7,Magic (2/5)), children = [Tree {root = (3,Fireball (1/3)), children = [Tree {root = (1,Flamewall (0/1)), children = []}]},Tree {root = (1,Levitation (1/2)), children = []}]}
annotateNodes :: Tree SkillNode -> Tree (Int, SkillNode)
annotateNodes t@(Tree root' children') = Tree (countRemainingPoints t, root') (annotateNodes <$> children')

-- |
-- >>> annotateNodes' sampleTree
-- Tree {root = (7,Magic (2/5)), children = [Tree {root = (3,Fireball (1/3)), children = [Tree {root = (1,Flamewall (0/1)), children = []}]},Tree {root = (1,Levitation (1/2)), children = []}]}
annotateNodes' :: Tree SkillNode -> Tree (Int, SkillNode)
annotateNodes' = fmap go . duplicate
  where
    go t = (countRemainingPoints t, root t)

-- |
-- >>> annotateNodes'' sampleTree
-- Tree {root = (7,Magic (2/5)), children = [Tree {root = (3,Fireball (1/3)), children = [Tree {root = (1,Flamewall (0/1)), children = []}]},Tree {root = (1,Levitation (1/2)), children = []}]}
annotateNodes'' :: Tree SkillNode -> Tree (Int, SkillNode)
annotateNodes'' = extend (\t -> (countRemainingPoints t, extract t))
-- annotateNodes'' = extend (countRemainingPoints &&& extract)
