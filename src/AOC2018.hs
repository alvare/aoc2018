{-# LANGUAGE OverloadedStrings #-}
module AOC2018 where

import           Control.Monad (mapM_)
import           Data.Char (ord)
import           Data.List (find, foldl')
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
--import qualified Data.ByteString as B
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  input <- B.readFile "data/input.txt"

  putStrLn "Hash:"
  putStrLn . show $ hashIds input

  putStrLn "Boxes:"
  putStrLn $ findBoxes input


hashIds :: ByteString -> Int
hashIds = sumAndMult . B.split '\n'


sumAndMult :: [ByteString] -> Int
sumAndMult list = twices * thrices
  where
    has n l = if n `V.elem` l then 1 else 0
    counts = map countChars list
    sumIfHas (a, b) l = (a + has 2 l, b + has 3 l)
    (twices, thrices) = foldl' sumIfHas (0, 0) counts


countChars :: ByteString -> Vector Int
countChars bs = V.modify reduceList $ V.replicate 256 0
  where
    reduceList l = mapM_ (M.modify l (+1) . ord) (B.unpack bs)


findBoxes :: ByteString -> String
findBoxes = format
          . maybe (error "nope") id
          . find differByOne
          . combinations
          . V.init
          . V.fromList
          . B.split '\n'
  where
    format (a, b) = B.unpack a ++ "\n" ++ B.unpack b

combinations :: Vector ByteString -> [(ByteString, ByteString)]
combinations strings = concat $ V.imap (\i e ->
  zip (repeat e) (V.toList . V.drop (i + 1) $ strings)) strings

differByOne :: (ByteString, ByteString) -> Bool
differByOne (l, r) = all (<=1) . scanl (\c e -> if e then c else c + 1) 0 . map (uncurry (==)) $ B.zip l r
