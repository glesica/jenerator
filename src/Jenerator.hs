{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Jenerator
    ( Date(..)
    , Page(..)
    , pageFromFilename
    , parseDate
    , parseTags
    , parseTitle
    , slugifyTitle
    ) where

import CMark (commonmarkToHtml)
import Data.Data
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Typeable
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
  } deriving (Show, Data, Typeable)

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

