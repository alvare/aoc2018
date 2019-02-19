module Day4 where

import           Data.Maybe            (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer hiding (space)

type Parser = Parsec Void Text

data LineType = Begins | Wakes | Falls
  deriving (Eq, Ord, Show)

data Line = Line
  { lineDay   :: String
  , lineTime  :: (Int, Int)
  , lineGuard :: Maybe Int
  , lineType  :: LineType
  } deriving (Show, Eq)

parseLine :: Text -> Line
parseLine = fromJust . parseMaybe parser

parser :: Parser Line
parser = do
  date' <- char '[' *> date
  time' <- time <* char ']'
  space
  (guard', type') <- guard <|> falls <|> wakes
  many $ satisfy (const True)
  pure $ Line date' time' guard' type'
  where
    date = some (alphaNumChar <|> char '-') <* space
    time = (,) <$> decimal <*> (char ':' *> decimal)
    guard = (,) <$> (Just <$> (string "Guard #" *> decimal)) <*> (pure Begins)
    falls = (,) <$> (string "falls asleep" *> pure Nothing) <*> pure Falls
    wakes = (,) <$> (string "wakes up" *> pure Nothing) <*> pure Wakes
