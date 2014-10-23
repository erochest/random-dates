{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Opts


main :: IO ()
main = print =<< execParser opts
