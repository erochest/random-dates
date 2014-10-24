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
generateData n mean stdev mc = withSystemRandom $ go n (createStartTable mc) M.empty
    where
        go 0 _     _     _ = return []
        go n table cache g = do
            (cache', title)  <-  fmap (fmap T.unwords . lastCache)
                             .   generateText_ g table cache mc
                             .   pos
                             .   truncate
                             =<< normal 8 3 g
            (cache'', descr) <-  fmap (fmap unparas . lastCache)
                             .   generateParagraphs g mc 50 10
                             .   pos
                             .   truncate
                             =<< normal 10 7 g
            date <- randomDate g mean stdev
            print (n, title, date, T.length descr)
            (DateRow title descr date:) <$> go (pred n) table cache'' g

        unparas = T.intercalate "\n\n" . map T.unwords
