module Comonad.Env.Formatter where

import Comonad.Env
import Control.Comonad

data Settings = Settings
  { padAmount :: Int,
    maxLength :: Int,
    padChar :: Char
  }
  deriving (Show)

settings :: Env Settings String
settings =
  env
    ( Settings
        { padAmount = 3,
          maxLength = 5,
          padChar = '*'
        }
    )
    "Hello World!"

getPadChar :: Env Settings a -> Char
getPadChar = asks padChar

setPadChar :: Char -> Settings -> Settings
setPadChar c s = s {padChar = c}

trunc :: Env Settings [a] -> [a]
trunc w =
  let mxLngth = asks maxLength w
   in take mxLngth (extract w)

-- |
-- >>> pad settings
-- "***Hello World!***"
pad :: Env Settings String -> String
pad w =
  let padAmt = asks padAmount w
      c = asks padChar w
      txt = extract w
      padding = replicate padAmt c
   in padding <> txt <> padding

-- | "do"-notation for env
pad2 :: Env Settings String -> String
pad2 = do
  -- Monad ((->) r)
  padding <- replicate <$> asks padAmount <*> asks padChar
  txt <- extract
  return $ padding <> txt <> padding

pad3 :: Env Settings String -> String
pad3 = do
  -- Applicative ((->) r)
  let padding = replicate <$> asks padAmount <*> asks padChar
  padding <> extract <> padding

-- |
-- >>> pipeline settings
-- "***Hello***"
pipeline :: Env Settings String -> String
pipeline = trunc =>= pad

-- |
-- >>> pipeline2 settings
-- "***He"
pipeline2 :: Env Settings String -> String
pipeline2 = pad =>= trunc

-- |
-- >>> pipeline3 settings
-- "***___Hello___***"
pipeline3 :: Env Settings String -> String
pipeline3 = trunc =>= pad . local (setPadChar '_') =>= pad

--                    ^^^ local only changes the environment for pad

-- local :: (e -> e') -> Env e a -> Env e' a
