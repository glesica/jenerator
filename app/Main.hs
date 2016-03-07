{-# LANGUAGE OverloadedStrings #-}
module Main where

import Jenerator
import Jenerator.IO

main :: IO ()
main = buildSite defaultSite
