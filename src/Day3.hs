module Day3 where

import           Control.Arrow               ((***))
import           Control.Monad               (join)
import           Control.Monad.Primitive     (PrimState)
import           Data.Foldable               (for_)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Data.Text.Read              (decimal)
import           Data.Vector.Unboxed         (MVector, Vector)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M


data Line = Line
  { lineId   :: Int
  , linePos  :: (Int, Int)
  , lineSize :: (Int, Int)
  } deriving (Show, Eq)


main :: IO ()
main = do
  input <- T.readFile "data/day3.txt"
  let size = 1000
  let lines' = map parse $ T.lines input

  square <- M.replicate (size * size) (0 :: Int)
  mapM_ (renderCuts square size) lines'
  v <- V.freeze square

  print .
    V.length .
    V.filter (>1) $
    v

  print . head . filter (allSpotsAreOne v size) $ lines'


coordsToPos :: Int -> (Int, Int) -> Int
coordsToPos size (x, y) = x + y * size


allSpotsAreOne :: Vector Int -> Int -> Line -> Bool
allSpotsAreOne v size (Line _ (x, y) (w, h)) =
  forAll [0..w-1] $ \i ->
    forAll [0..h-1] $ \j ->
      let c = v V.! (coordsToPos size (x+i, y+j))
      in c == 1
  where
    forAll = flip all


renderCuts :: MVector (PrimState IO) Int -> Int -> Line -> IO ()
renderCuts v size (Line _ (x, y) (w, h)) = do
  for_ [0..w-1] $ \i ->
    for_ [0..h-1] $ \j ->
      M.modify v (+1) (coordsToPos size (x+i, y+j))


parse :: Text -> Line
parse line = let [i, "@", p, s] = T.splitOn " " line
             in Line { lineId=readId i
                     , linePos=readPos p
                     , lineSize=readSize s }
  where
    parseNumber = fst . either undefined id . decimal
    readTuple t s = let [x, y] = T.splitOn t s in join (***) parseNumber (x, y)
    readId = parseNumber . T.tail
    readPos = readTuple "," . T.init
    readSize = readTuple "x"
