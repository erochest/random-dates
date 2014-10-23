{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}


module RandomDates.Random
    ( randomDate
    , generateData
    ) where


import           Control.Applicative
import           Control.Monad
import           Data.Thyme
import           System.Random.MWC
import           System.Random.MWC.Distributions

import           RandomDates.Types


randomDate :: Day -> Int -> GenIO -> IO Day
randomDate (ModifiedJulianDay d) stdev g =
    ModifiedJulianDay . truncate <$> normal (fromIntegral d) (fromIntegral stdev) g

generateData :: Int -> Day -> Int -> IO [DateRow]
generateData n mean stdev = withSystemRandom $
    replicateM n . fmap (DateRow "" "") . randomDate mean stdev
