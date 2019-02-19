module Day4Spec where

import           Test.Hspec
import           Test.QuickCheck

import           Day4

spec :: Spec
spec = do
  describe "parse" $ do

    it "parses a begin" $ do
      let line = "[1518-04-10 23:52] Guard #3559 begins shift"
      parseLine line === Line { lineDay="1518-04-10"
                              , lineTime=(23, 52)
                              , lineGuard=Just 3559
                              , lineType=Begins }

    it "parses a falls" $ do
      let line = "[1518-04-10 23:52] falls asleep"
      parseLine line === Line { lineDay="1518-04-10"
                              , lineTime=(23, 52)
                              , lineGuard=Nothing
                              , lineType=Falls }

    it "parses a wakes" $ do
      let line = "[1518-04-10 23:52] wakes up"
      parseLine line === Line { lineDay="1518-04-10"
                              , lineTime=(23, 52)
                              , lineGuard=Nothing
                              , lineType=Wakes }
