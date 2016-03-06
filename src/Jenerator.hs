{-# LANGUAGE OverloadedStrings #-}
module Jenerator
    ( Date(..)
    , Page(..)
    , buildPage
    , buildPageAtFilename
    , pageFromFilename
    , parseDate
    , parseTags
    , parseTitle
    ) where

import CMark (commonmarkToHtml)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Text.IO as TI
import System.FilePath.Posix

data Date = Date
  Int -- Year
  Int -- Month
  Int -- Day
  deriving (Eq, Show)

data Page = Page
  { srcPath :: FilePath
  , date    :: Date
  , tags    :: [String]
  , title   :: String
  } deriving (Show)

buildPageAtFilename :: FilePath -> FilePath -> IO ()
buildPageAtFilename destDir = buildPage destDir . pageFromFilename

buildPage :: FilePath -> Page -> IO ()
buildPage destDir page = do
  let inPath = srcPath page
  let outPath = destDir </> (slugifyTitle $ title page) ++ ".html"
  srcData <- TI.readFile inPath
  TI.writeFile outPath $ commonmarkToHtml [] srcData

pageFromFilename :: FilePath -> Page
pageFromFilename filename = do
  let [tagStr, dateStr, titleStr] = splitOn "__" $ dropExtension filename
  Page {
    srcPath = filename,
    date    = parseDate dateStr,
    tags    = parseTags tagStr,
    title   = parseTitle titleStr
  }

parseDate :: String -> Date
parseDate dateStr = do
  let year:month:day:_ = splitOn "-" dateStr
  Date (read year :: Int) (read month :: Int) (read day :: Int)

parseTags :: String -> [String]
parseTags = splitOn "_"

parseTitle :: String -> String
parseTitle = unwords . splitOn "_"

slugifyTitle :: String -> String
slugifyTitle = intercalate "-" . words

