{-# LANGUAGE OverloadedStrings #-}

module Jenerator.IO
    ( buildPage
    , buildPageAtFilename
    ) where

import CMark (commonmarkToHtml)
import qualified Data.Text.IO as TI
import System.FilePath.Posix
import Jenerator

buildPageAtFilename :: FilePath -> FilePath -> IO ()
buildPageAtFilename destDir = buildPage destDir . pageFromFilename

buildPage :: FilePath -> Page -> IO ()
buildPage destDir page = do
  let inPath = srcPath page
  let outPath = destDir </> (slugifyTitle $ title page) ++ ".html"
  srcData <- TI.readFile inPath
  TI.writeFile outPath $ commonmarkToHtml [] srcData

