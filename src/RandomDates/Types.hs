{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}


module RandomDates.Types
    ( RandomOpts(..)
    , optN
    , optCenterDate
    , optStandardDev

    , DateRow
    , rowTitle
    , rowDescr
    , rowDate
    ) where


import           Control.Lens
import qualified Data.ByteString.Char8       as B
import           Data.CSV.Conduit.Conversion
import qualified Data.Map.Strict             as M
import qualified Data.Text                   as T
import           Data.Text.Encoding
import           Data.Thyme
import           System.Locale


data RandomOpts = RandomOpts
                { _optN           :: !Int
                , _optCenterDate  :: !Day
                , _optStandardDev :: !Int
                } deriving (Show)
makeLenses ''RandomOpts

data DateRow = DateRow
             { _rowTitle :: !T.Text
             , _rowDescr :: !T.Text
             , _rowDate  :: !Day
             } deriving (Show)
makeLenses ''DateRow

instance ToNamedRecord DateRow where
    toNamedRecord DateRow{..} =
        M.fromList [ ("title"       , encodeUtf8 _rowTitle)
                   , ("description" , encodeUtf8 _rowDescr)
                   , ("date"        , format _rowDate)
                   ]
        where
            format = B.pack . formatTime defaultTimeLocale "%Y-%m-%d"
