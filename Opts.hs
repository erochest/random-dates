

module Opts
    ( opts
    , execParser
    ) where


import           Control.Applicative
import           Data.Thyme
import           Options.Applicative
import           Options.Applicative.Types
import           System.Locale

import           RandomDates.Types


opts' :: Parser RandomOpts
opts' = RandomOpts
      <$> option auto (  short 'n' <> long "n" <> metavar "N" <> value 100
                      <> help "The number of items to produce. Default = 100.")
      <*> option auto (  short 'd' <> long "day" <> metavar "YYYY-MM-DD"
                      <> help "The date to center the random dates on.")
      <*> option auto (  short 's' <> long "std-dev" <> metavar "INTEGER" <> value 365
                      <> help "The standard deviations (in days) \
                              \for generating random dates. Default = 365.")
      <*> strOption   (  short 'o' <> long "output" <> metavar "FILENAME"
                      <> help "The file to write the random data to.")

opts :: ParserInfo RandomOpts
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Generate random data with dates."
            <> header "random-dates --- generate random CSV data with dates.")
