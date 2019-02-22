module Day4 where

import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
                                         hiding ( space )

type Parser = Parsec Void Text

data LineType = Begins Int | Wakes | Falls
  deriving (Eq, Ord, Show)

data Line = Line
  { lineDay   :: String
  , lineTime  :: (Int, Int)
  , lineType  :: LineType
  } deriving (Show, Eq)

parseLine :: Text -> Line
parseLine = fromJust . parseMaybe parser

parser :: Parser Line
parser = Line <$> date <*> time <*> (guard <|> falls <|> wakes) <* space
 where
  date  = char '[' *> some (alphaNumChar <|> char '-') <* space
  time  = (,) <$> decimal <*> (char ':' *> decimal <* char ']' <* space)
  guard = string "Guard #" *> (Begins <$> decimal) <* " begins shift"
  falls = string "falls asleep" *> pure Falls
  wakes = string "wakes up" *> pure Wakes
