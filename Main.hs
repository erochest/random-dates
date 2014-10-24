{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Applicative
import qualified Data.Text.IO        as TIO

import           RandomDates.Output
import           RandomDates.Random
import           RandomDates.Text
import           RandomDates.Types

import           Opts


main :: IO ()
main = do
    rdOpts@RandomOpts{..} <- execParser opts
    markov <- trainChains . tokenize <$> TIO.readFile _optTextFile
    writeRows _optOutput =<< generateData _optN _optCenterDate _optStandardDev markov
