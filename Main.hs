{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           RandomDates.Random
import           RandomDates.Types

import           Opts


main :: IO ()
main = do
    rdOpts@RandomOpts{..} <- execParser opts
    undefined
