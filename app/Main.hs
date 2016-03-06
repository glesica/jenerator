{-# LANGUAGE OverloadedStrings #-}
module Main where

import Jenerator.IO

main :: IO ()
main = buildPageAtFilename "example/" "example/tag0_tag1__2016-03-05__Test_document.md"
