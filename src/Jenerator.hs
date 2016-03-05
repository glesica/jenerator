{-# LANGUAGE OverloadedStrings #-}
module Jenerator
    ( Date(..)
    , Page(..)
    , buildMarkdown
    , parseDate
    , parseTags
    , parseTitle
    , replaceExt
    , stripExt
    ) where

import CMark (commonmarkToHtml)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Text.IO as TI

data Date = Date
  Int -- Year
  Int -- Month
  Int -- Day
  deriving (Eq, Show)

data Page = Page {
  filePath :: FilePath,
  date :: Date,
  tags :: [String],
  title :: String
} deriving (Show)

parseDate :: String -> Date
parseDate dateStr = do
  let year:month:day:_ = splitOn "-" dateStr
  Date (read year :: Int) (read month :: Int) (read day :: Int)

parseTags :: String -> [String]
parseTags = splitOn "_"

parseTitle :: String -> String
parseTitle = unwords . splitOn "_"

buildMarkdown :: FilePath -> IO ()
buildMarkdown inPath = do
  mdData <- TI.readFile inPath
  let outPath = replaceExt "html" inPath
  TI.writeFile outPath $ commonmarkToHtml [] mdData

stripExt :: FilePath -> String
stripExt = intercalate "." . init . splitOn "."

replaceExt :: String -> FilePath -> FilePath 
replaceExt newExt filename = (stripExt filename) ++ "." ++ newExt

