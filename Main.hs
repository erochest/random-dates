{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           RandomDates.Output
import           RandomDates.Random
import           RandomDates.Types

import           Opts


main :: IO ()
main = do
    rdOpts@RandomOpts{..} <- execParser opts
    writeRows _optOutput =<< generateData _optN _optCenterDate _optStandardDev
