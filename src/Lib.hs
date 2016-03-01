{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import CMark
import qualified Data.Text as T

someFunc :: T.Text -> IO ()
someFunc = putStrLn . T.unpack . commonmarkToHtml []
