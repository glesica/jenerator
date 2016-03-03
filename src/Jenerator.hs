{-# LANGUAGE OverloadedStrings #-}
module Jenerator
    ( 
      Page(..),
      buildMarkdown
    ) where

import CMark (commonmarkToHtml)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Text.IO as TI

data Page = Page {
  title :: String,
  pubYear :: Int,
  pubMonth :: Int,
  pubDay :: Int,
  tags :: [String],
  filePath :: FilePath
} deriving (Show)

buildMarkdown :: FilePath -> IO ()
buildMarkdown inPath = do
  mdData <- TI.readFile inPath
  let outPath = replaceExt "html" inPath
  TI.writeFile outPath $ commonmarkToHtml [] mdData

replaceExt :: String -> FilePath -> FilePath 
replaceExt newExt filename = do
  let basename = intercalate "." $ init $ splitOn "." filename
  basename ++ "." ++ newExt

