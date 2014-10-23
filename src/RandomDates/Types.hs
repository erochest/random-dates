{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module RandomDates.Types
    ( RandomOpts(..)
    , optN
    , optCenterDate
    , optStandardDev
    ) where


import           Control.Lens
import           Data.Thyme


data RandomOpts = RandomOpts
                { _optN           :: !Int
                , _optCenterDate  :: !Day
                , _optStandardDev :: !Int
                } deriving (Show)
makeLenses ''RandomOpts
