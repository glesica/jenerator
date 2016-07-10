{-# LANGUAGE OverloadedStrings #-}

module Jenerator.IO
    ( buildPage
    , buildSite
    ) where

import System.Directory (getDirectoryContents)
import System.FilePath.Posix
import Jenerator

buildSite :: Site -> IO ()
buildSite site = do
  pageFilenames <- listDirectory $ pagesPath site
  let pages = [pageFromFilename filename | filename <- pageFilenames]
  buildPages site pages

buildPages :: Site -> [Page] -> IO ()
buildPages site [] = return ()
buildPages site (page:pages) = do
  buildPage site page
  buildPages site pages

buildPage :: Site -> Page -> IO ()
buildPage site page = do
  putStr $ "Building page: " ++ title page ++ "."
  let inPath = pagesPath site </> srcPath page
  let outPath = buildPath site </> slugifyTitle (title page) ++ ".html"
  inData <- readFile inPath
  tmplData <- readFile $ pageTmplPath site
  let outData = renderPage tmplData page
  writeFile outPath outData
  putStr " Done."

-- Add this because it isn't in the version of directory we're using yet
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
    where f filename = filename /= "." && filename /= ".."

