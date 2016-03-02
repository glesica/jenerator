{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( buildMarkdown
    ) where

import CMark (commonmarkToHtml)
import qualified Data.Text as T
import qualified Data.Text.IO as TI

buildMarkdown :: FilePath -> IO ()
buildMarkdown inPath = do
  mdData <- TI.readFile inPath
  let outPath = T.unpack $ rext "html" $ T.pack inPath
  TI.writeFile outPath $ commonmarkToHtml [] mdData

rext :: T.Text -> T.Text -> T.Text
rext newExt filename = do
  let basename = T.intercalate "." $ init $ T.splitOn "." filename
  T.concat [basename, ".", newExt]

