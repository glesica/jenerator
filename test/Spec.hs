module Main where

import Jenerator
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "pageFromFilename" $ do
    it "should return a valid page with one tag" $ do
      let filename = "tag0__2000-01-02__title.ext"
      let p = pageFromFilename filename
      (srcPath p) `shouldBe` filename
      (date p) `shouldBe` (Date 2000 1 2)
      (tags p) `shouldBe` ["tag0"]
      (title p) `shouldBe` "title"

    it "should return a valid page with two tags" $ do
      let filename = "tag0_tag1__2000-01-02__title.ext"
      let p = pageFromFilename filename
      (srcPath p) `shouldBe` filename
      (date p) `shouldBe` (Date 2000 1 2)
      (tags p) `shouldBe` ["tag0", "tag1"]
      (title p) `shouldBe` "title"

    it "should return a valid page with a long title" $ do
      let filename = "tag0__2000-01-02__long_title.ext"
      let p = pageFromFilename filename
      (srcPath p) `shouldBe` filename
      (date p) `shouldBe` (Date 2000 1 2)
      (tags p) `shouldBe` ["tag0"]
      (title p) `shouldBe` "long title"

  describe "defaultSite" $
    it "should return a valid site config" $ do
      let s = defaultSite
      (tmplPath s) `shouldBe` "template.html"
      (pagesPath s) `shouldBe` "pages"
      (staticPath s) `shouldBe` "static"
      (buildPath s) `shouldBe` "build"

  describe "parseDate" $ do
    it "should parse a zero-padded date" $ do
      let d = parseDate "2000-01-02"
      d `shouldBe` (Date 2000 1 2)

    it "should parse a non zero-padded date" $ do
      let d = parseDate "2000-1-2"
      d `shouldBe` (Date 2000 1 2)

  describe "parseTags" $
    it "should parse simple tags" $ do
      let t = parseTags "tag0_tag1"
      t `shouldBe` ["tag0", "tag1"]

  describe "parseTitle" $ do
    it "should replace underscores with spaces" $ do
      let t = parseTitle "a_b"
      t `shouldBe` "a b"

    it "should handle unicode characters" $ do
      let t = parseTitle "✔_✘"
      t `shouldBe` "✔ ✘"

  describe "slugifyTitle" $ do
    it "should slugify a on-word title" $ do
      let s = slugifyTitle "title"
      s `shouldBe` "title"

    it "should slugify a longer title" $ do
      let s = slugifyTitle "longer title"
      s `shouldBe` "longer-title"

