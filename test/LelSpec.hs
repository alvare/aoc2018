module LelSpec where

import Test.Hspec
import Test.QuickCheck

import AOC2018

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      "lel" === "lel"
