{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module Comonad.Transformers.Report where

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Traced
import Data.Function ((&))

data ReportStyle = Detailed | Summary

projections :: Sum Int -> Double
projections (Sum month) = 1.2 ^ (max 0 month) * 100

reportConfig :: EnvT ReportStyle (Traced (Sum Int)) Double
reportConfig = EnvT Detailed (traced projections)

previousMonth :: ComonadTraced (Sum Int) w => w a -> a
previousMonth = trace (Sum (-1))

nextMonth :: ComonadTraced (Sum Int) w => w a -> a
nextMonth = trace (Sum 1)

-- |
-- >>> putStrLn $ detailedReport reportConfig
-- This months sales in totality are: 100.0
-- Previous month's sales: 100.0
-- Next month's projections: 120.0
detailedReport :: (ComonadTraced (Sum Int) w) => w Double -> String
detailedReport = do
  -- reader monad
  salesAmt <- extract
  prev <- previousMonth
  next <- nextMonth
  return $
    unlines
      [ "This months sales in totality are: " <> show salesAmt,
        "Previous month's sales: " <> show prev,
        "Next month's projections: " <> show next
      ]

buildHeader :: (ComonadEnv ReportStyle w) => w a -> String
buildHeader = do
  style <- ask
  return $ case style of
    Detailed -> "Please find enclosed your DETAILED report: \n"
    Summary -> "Please find enclosed your SUMMARY report: \n"

buildReport :: (ComonadTraced (Sum Int) w, ComonadEnv ReportStyle w) => w Double -> String
buildReport = do
  -- reader monad
  header <- buildHeader
  salesAmt <- extract
  style <- ask
  case style of
    Summary -> return $ header <> "We achieved " <> show salesAmt <> " in sales!"
    Detailed -> do
      rpt <- detailedReport
      return $ header <> rpt

-- >>> putStrLn $ reportExample 4
-- Please find enclosed your DETAILED report:
-- This months sales in totality are: 207.35999999999999
-- Previous month's sales: 172.8
-- Next month's projections: 248.832
reportExample :: Int -> String
reportExample month = reportConfig =>> buildReport & trace (Sum month)

-- >>> putStrLn $ summaryReportExample 4
-- Please find enclosed your SUMMARY report:
-- We achieved 207.35999999999999 in sales!
summaryReportExample :: Int -> String
summaryReportExample month = local (const Summary) reportConfig =>> buildReport =>> trace (Sum month) & extract
