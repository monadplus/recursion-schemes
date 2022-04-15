{-# LANGUAGE FlexibleContexts #-}

module Comonad.Transformers.ReportAdvanced where

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Store
import Control.Comonad.Traced
import Data.Function ((&))

data ReportStyle = Detailed | Summary

data Region = America | UK | Germany
  deriving (Show, Eq, Ord)

projections :: Region -> Sum Int -> Float
projections UK (Sum month) = 1.2 ^ (max 0 month) * 100
projections America (Sum month) = 1.3 ^ (max 0 month) * 200
projections Germany (Sum month) = 1.5 ^ (max 0 month) * 300

reportConfig :: EnvT ReportStyle (TracedT (Sum Int) (Store Region)) Float
reportConfig = EnvT Detailed (TracedT (store projections UK))

previousMonth :: ComonadTraced (Sum Int) w => w a -> a
previousMonth = trace (Sum (-1))

nextMonth :: ComonadTraced (Sum Int) w => w a -> a
nextMonth = trace (Sum 1)

detailedReport :: (ComonadTraced (Sum Int) w, ComonadStore Region w) => w Float -> String
detailedReport = do
  salesAmt <- extract
  prev <- previousMonth
  next <- nextMonth
  region <- pos
  return $
    unlines
      [ show region <> ":",
        "This months sales in totality are: " <> show salesAmt,
        "Previous month's sales: " <> show prev,
        "Next month's projections: " <> show next
      ]

buildHeader :: (ComonadEnv ReportStyle w) => w a -> String
buildHeader = do
  style <- ask
  return $ case style of
    Detailed -> "Please find enclosed your DETAILED report\n"
    Summary -> "Please find enclosed your SUMMARY report\n"

buildReport :: (ComonadTraced (Sum Int) w, ComonadEnv ReportStyle w, ComonadStore Region w) => w Float -> String
buildReport = do
  header <- buildHeader
  salesAmt <- extract
  style <- ask
  case style of
    Summary -> return $ header <> "We achieved " <> show salesAmt <> " in sales!"
    Detailed -> do
      rpt <- detailedReport
      compReport <- comparisonReport
      return $ header <> rpt <> "\n" <> compReport
  where
    comparisonReport ::
      (ComonadTraced (Sum Int) w, ComonadStore Region w) =>
      w Float ->
      String
    comparisonReport w =
      let otherReports = w =>> detailedReport =>> otherRegions & extract
       in "Comparison Report\n" <> unlines otherReports

    otherRegions :: (ComonadStore Region w) => w a -> [a]
    otherRegions = experiment others
      where
        others currentRegion = filter (/= currentRegion) allRegions

    allRegions :: [Region]
    allRegions = [UK, America, Germany]

-- >>> putStrLn $ reportExample 1
-- Please find enclosed your DETAILED report
-- UK:
-- This months sales in totality are: 120.00001
-- Previous month's sales: 100.0
-- Next month's projections: 144.0
--
-- Comparison Report
-- America:
-- This months sales in totality are: 260.0
-- Previous month's sales: 200.0
-- Next month's projections: 337.99997
--
-- Germany:
-- This months sales in totality are: 450.0
-- Previous month's sales: 300.0
-- Next month's projections: 675.0
reportExample :: Int -> String
reportExample month = reportConfig =>> buildReport & trace (Sum month)

-- >>> putStrLn $ summaryReportExample 1
-- Please find enclosed your SUMMARY report
-- We achieved 120.00001 in sales!
summaryReportExample :: Int -> String
summaryReportExample month = reportConfig =>> buildReport . local (const Summary) & trace (Sum month)
