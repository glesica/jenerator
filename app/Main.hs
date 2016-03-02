{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

main :: IO ()
main = buildMarkdown "README.md"
