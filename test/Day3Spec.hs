module Day3Spec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Vector.Unboxed.Mutable as M

import Day3

spec :: Spec
spec = do
  describe "parse" $ do

    it "parses ok" $ do
      parse "#7 @ 321,868: 13x17" === Line { lineId=7
                                           , linePos=(321, 868)
                                           , lineSize=(13, 17) }

  describe "renderCuts" $ do

    it "works on 0" $ do
      v <- M.replicate (10 * 10) (0 :: Int)
      renderCuts v 10 Line { lineId=7
                           , linePos=(3, 2)
                           , lineSize=(5, 4) }
      i <- M.read v (coordsToPos 10 (0, 0))
      i `shouldBe` 0

    it "works on 1" $ do
      v <- M.replicate (10 * 10) (0 :: Int)
      renderCuts v 10 Line { lineId=7
                           , linePos=(3, 2)
                           , lineSize=(5, 4) }
      i <- M.read v (coordsToPos 10 (3, 2))
      i `shouldBe` 1

    it "works on 2" $ do
      v <- M.replicate (10 * 10) (0 :: Int)
      renderCuts v 10 Line { lineId=7
                           , linePos=(3, 2)
                           , lineSize=(5, 4) }
      renderCuts v 10 Line { lineId=7
                           , linePos=(3, 2)
                           , lineSize=(5, 4) }
      i <- M.read v (coordsToPos 10 (3, 2))
      i `shouldBe` 2
