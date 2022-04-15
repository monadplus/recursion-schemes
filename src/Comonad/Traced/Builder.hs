module Comonad.Traced.Builder where

import Comonad.Traced
import Control.Comonad
import Data.Function ((&))
import qualified Data.Map as M
import Data.Monoid

-------------------------------------------------------------
-- Example 1

newBuilder :: Traced [String] String
newBuilder = traced concat

append :: String -> Traced [String] String -> String
append s = trace [s]

-- |
-- >>> logMsg newBuilder
-- "worldhello "
logMsg :: Traced [String] String -> String
logMsg = append "hello " =>= append "world"

-- How to fix this?

betterBuilder :: Traced (Dual [String]) String
betterBuilder = traced (concat . getDual)

betterAppend :: String -> Traced (Dual [String]) String -> String
betterAppend s = trace (Dual [s])

-- |
-- >>> betterLogMsg betterBuilder
-- "hello world"
betterLogMsg :: Traced (Dual [String]) String -> String
betterLogMsg = betterAppend "hello " =>= betterAppend "world"

-------------------------------------------------------------
-- Example 2

keyedLogger :: Traced (M.Map String String) String
keyedLogger = traced render
  where
    render m = foldMap pairToStr (M.toList m)
    pairToStr (k, v) = k <> ": " <> v <> "\n"

addLog :: String -> String -> Traced (M.Map String String) String -> String
addLog k v = trace (M.singleton k v)

-- |
-- >>> putStrLn keyedMsg
-- msg: balance not sufficient
-- userName: Joe
keyedMsg :: String
keyedMsg =
  keyedLogger
    & ( addLog "userName" "Joe"
          =>= addLog "msg" "balance not sufficient"
      )
  -- extract $ keyedLogger =>> addLog "userName" "Joe" =>> addLog "msg" "balance not sufficient"
