module Day3 where

import Control.Arrow ((***))
import Control.Monad (join, filterM, when)
import Control.Monad.Primitive (PrimState)
import Data.IORef
import Data.Text (Text)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector.Unboxed (Vector, MVector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Text.Read


data Line = Line
  { lineId :: Int
  , linePos :: (Int, Int)
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

  unique <- head <$> filterM (isUnique v size) lines'
  print unique


coordsToPos :: Int -> (Int, Int) -> Int
coordsToPos size (x, y) = x + y * size


isUnique :: Vector Int -> Int -> Line -> IO Bool
isUnique v size (Line _ (x, y) (w, h)) = do
  ref <- newIORef True
  for_ [0..w-1] $ \i ->
    for_ [0..h-1] $ \j ->
      let z = v V.! (coordsToPos size (x+i, y+j))
      in when (z > 1) $
        writeIORef ref False
  readIORef ref



renderCuts :: MVector (PrimState IO) Int -> Int -> Line -> IO ()
renderCuts v size (Line _ (x, y) (w, h)) = do
  for_ [0..w-1] $ \i ->
    for_ [0..h-1] $ \j ->
      M.modify v (+1) (coordsToPos size (x+i, y+j))


parse :: Text -> Line
parse line = case T.splitOn " " line of
            [i, "@", p, s] -> Line { lineId=readId i
                                   , linePos=readPos p
                                   , lineSize=readSize s }
            _ -> error "nope 1"
  where
    parseNumber = fst . either undefined id . decimal
    readTuple t s = case T.splitOn t s of
                      [x, y] -> join (***) parseNumber (x, y)
                      _ -> error "nope 2"
    readId = parseNumber . T.tail
    readPos s = readTuple "," (T.init s)
    readSize s = readTuple "x" s
