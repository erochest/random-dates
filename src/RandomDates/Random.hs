{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}


module RandomDates.Random
    ( randomDate
    ) where


import           Control.Applicative
import           Data.Thyme
import           System.Random.MWC
import           System.Random.MWC.Distributions

import           RandomDates.Types


randomDate :: Day -> Int -> GenIO -> IO Day
randomDate (ModifiedJulianDay d) stdev g =
    ModifiedJulianDay . truncate <$> normal (fromIntegral d) (fromIntegral stdev) g
