{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Jenerator
    ( Date(..)
    , Page(..)
    , Site(..)
    , defaultSite
    , pageFromFilename
    , parseDate
    , parseTags
    , parseTitle
    , renderPage
    , slugifyTitle
    ) where

import Data.Aeson
import Data.Data
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Typeable
import System.FilePath.Posix
import Text.Pandoc.Templates

data Date = Date
  Int -- Year
  Int -- Month
  Int -- Day
  deriving (Eq, Show, Data, Typeable)

data Page = Page
  { srcPath :: FilePath
  , date    :: Date
  , tags    :: [String]
  , title   :: String
  , content :: String
  } deriving (Show, Data, Typeable)

instance ToJSON Page where
  toJSON e = do
    let Date year month day = date e
    object [ "srcPath" .= srcPath e
           , "date" .= object [ "year" .= year
                              , "month" .= month
                              , "day" .= day ]
           , "tags" .= tags e
           , "title" .= title e ]

data Site = Site
  { pageTmplPath  :: FilePath
  , indexTmplPath :: FilePath
  , pagesPath     :: FilePath
  , staticPath    :: FilePath
  , buildPath     :: FilePath
  } deriving (Show, Data, Typeable)

pageFromFilename :: FilePath -> Page
pageFromFilename filename = do
  let [tagStr, dateStr, titleStr] = splitOn "__" $ dropExtension filename
  Page {
    srcPath = filename,
    date    = parseDate dateStr,
    tags    = parseTags tagStr,
    title   = parseTitle titleStr,
    content = ""
  }

addContentToPage :: Page -> String -> Page
addContentToPage page content =
  Page {
    srcPath = srcPath page,
    date    = date page,
    tags    = tags page,
    title   = title page,
    content = content
  }

defaultSite :: Site
defaultSite = Site {
    pageTmplPath = "page.html",
    indexTmplPath = "index.html",
    pagesPath = "pages",
    staticPath = "static",
    buildPath = "build"
  }

renderPage :: String -> Page -> String
renderPage = renderTemplate'

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

