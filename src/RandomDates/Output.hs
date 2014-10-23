{-# LANGUAGE OverloadedStrings #-}


module RandomDates.Output
    ( writeRows
    ) where


import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List           as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import           System.IO

import           RandomDates.Types


writeRows :: FilePath -> [DateRow] -> IO ()
writeRows path rows = runResourceT $
       CL.sourceList rows
    $= CL.map toNamedRecord
    $= (writeHeaders settings >> fromCSV settings)
    $$ sinkFile path
    where
        settings = defCSVSettings
