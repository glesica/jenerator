{-# LANGUAGE OverloadedStrings #-}
module Main where

import Jenerator

main :: IO ()
main = buildMarkdown "README.md"
