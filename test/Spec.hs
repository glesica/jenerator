{-# LANGUAGE OverloadedStrings #-}

module Main where

import Jenerator
import Test.Hspec

main :: IO ()
main = hspec $ do
  let oneTagFilename = "tag0__2000-01-02__title.ext"
  let twoTagFilename = "tag0_tag1__2000-01-02__title.ext"
  let longTitleFilename = "tag0__2000-01-02__long_title.ext"

  describe "pageFromFilename" $ do
    it "should return a valid page with one tag" $ do
      let p = pageFromFilename oneTagFilename
      srcPath p `shouldBe` oneTagFilename
      date p `shouldBe` Date 2000 1 2
      tags p `shouldBe` ["tag0"]
      title p `shouldBe` "title"

    it "should return a valid page with two tags" $ do
      let p = pageFromFilename twoTagFilename
      srcPath p `shouldBe` twoTagFilename
      date p `shouldBe` Date 2000 1 2
      tags p `shouldBe` ["tag0", "tag1"]
      title p `shouldBe` "title"

    it "should return a valid page with a long title" $ do
      let p = pageFromFilename longTitleFilename
      srcPath p `shouldBe` longTitleFilename
      date p `shouldBe` Date 2000 1 2
      tags p `shouldBe` ["tag0"]
      title p `shouldBe` "long title"

  describe "defaultSite" $
    it "should return a valid site config" $ do
      let s = defaultSite
      pageTmplPath s `shouldBe` "page.html"
      indexTmplPath s `shouldBe` "index.html"
      pagesPath s `shouldBe` "pages"
      staticPath s `shouldBe` "static"
      buildPath s `shouldBe` "build"

  describe "parseDate" $ do
    it "should parse a zero-padded date" $ do
      let d = parseDate "2000-01-02"
      d `shouldBe` Date 2000 1 2

    it "should parse a non zero-padded date" $ do
      let d = parseDate "2000-1-2"
      d `shouldBe` Date 2000 1 2

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
    it "should slugify a one-word title" $ do
      let s = slugifyTitle "title"
      s `shouldBe` "title"

    it "should slugify a longer title" $ do
      let s = slugifyTitle "longer title"
      s `shouldBe` "longer-title"

  describe "renderPage" $ do
    it "should produce a page from an dummy template" $ do
      let p = pageFromFilename oneTagFilename
      let o = renderPage "blank" p
      o `shouldBe` "blank"

    it "should produce a page from a template with variables" $ do
      let p = pageFromFilename oneTagFilename
      let o = renderPage "title: $title$" p
      o `shouldBe` "title: title"

