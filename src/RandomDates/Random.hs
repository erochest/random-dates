{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}


module RandomDates.Random
    ( randomDate
    , generateData
    ) where


import           Control.Applicative
import           Control.Monad
import qualified Data.HashMap.Strict             as M
import qualified Data.Text                       as T
import           Data.Thyme
import           System.Random.MWC
import           System.Random.MWC.Distributions

import           RandomDates.Text
import           RandomDates.Types


randomDate :: GenIO -> Day -> Int -> IO Day
randomDate g (ModifiedJulianDay d) stdev =
    ModifiedJulianDay . truncate <$> normal (fromIntegral d) (fromIntegral stdev) g

generateData :: Int -> Day -> Int -> MarkovChains -> IO [DateRow]
generateData n mean stdev mc = withSystemRandom $ go (createStartTable mc) M.empty
    where
        go table cache g = do
            (cache', title)  <-  generateText_ g table cache mc
                             >>= fmap (fmap T.unwords . lastCache) . takeNormal g 8 3
            (cache'', descr) <-  generateParagraphs g mc 50 10
                             >>= fmap (fmap unparas . lastCache) . takeNormal g 10 7
            date <- randomDate g mean stdev
            (DateRow title descr date:) <$> go table cache'' g

        unparas = T.intercalate "\n\n" . map T.unwords
