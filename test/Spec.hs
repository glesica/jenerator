module Main where

import Jenerator
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseDate" $ do
    it "should parse a zero-padded date" $ do
      let date = parseDate "2000-01-02"
      date `shouldBe` Date 2000 1 2

    it "should parse a non zero-padded date" $ do
      let date = parseDate "2000-1-2"
      date `shouldBe` Date 2000 1 2

  describe "parseTags" $
    it "should parse simple tags" $ do
      let tags = parseTags "tag0_tag1"
      tags `shouldBe` ["tag0", "tag1"]

  describe "parseTitle" $ do
    it "should replace underscores with spaces" $ do
      let title = parseTitle "a_b"
      title `shouldBe` "a b"

    it "should handle unicode characters" $ do
      let title = parseTitle "✔_✘"
      title `shouldBe` "✔ ✘"

  describe "stripExt" $ do
    it "should strip the extension of a simple filename" $ do
      let basename = stripExt "file.ext"
      basename `shouldBe` "file"

    it "should strip the extension of a complex filename" $ do
      let basename = stripExt "complex.file.ext"
      basename `shouldBe` "complex.file"

  describe "replaceExt" $ do
    it "should replace the extension of a simple filename" $ do
      let newname = replaceExt "new" "file.ext"
      newname `shouldBe` "file.new"

    it "should replace the extension of a complex filename" $ do
      let newname = replaceExt "new" "complex.file.ext"
      newname `shouldBe` "complex.file.new"

