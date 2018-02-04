{-# LANGUAGE OverloadedStrings #-}
module CSVParse
    ( csv
    ) where

import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char hiding (separatorChar)

import CSVError

-- Convenient type alias: define our custom error type to be
-- CSVError, and we are parsing Text.
type Parser = Parsec CSVError T.Text

-- | Define the CSV separator char to be comma (',').
separatorChar :: Char
separatorChar = ','

-- | Parse the separator char.
separator :: Parser Char
separator = char separatorChar

-- | Is this the separator char?
isSeparatorChar :: Char -> Bool
isSeparatorChar = (==) separatorChar

-- | Is this a newline char ('\r' or '\n')?
isNewlineChar :: Char -> Bool
isNewlineChar c = c == '\r' || c == '\n'

-- | Is this a newline or separator character?
isCSVControlChar :: Char -> Bool
isCSVControlChar c = isNewlineChar c || isSeparatorChar c

-- | Parse a single CSV field, with any characters allowed except for
-- the separator char and end-of-line control characters.
field :: Parser T.Text
field = takeWhileP (Just "field text") (not . isCSVControlChar)

-- | Parse a CSV row, separated by the separator char, into Text fields.
row :: Parser [T.Text]
row = field `sepBy` separator

-- | Parse a strange end-of-line sequence ("\r\r\n"). This was found in the test data.
strangeEOL :: Parser T.Text
strangeEOL = string "\r\r\n"

-- | Parse a CSV end-of-line sequence. Can be one of '\n', "\r\n", or "\r\r\n".
csvEOL :: Parser T.Text
csvEOL = eol <|> strangeEOL <?> "end of line"

-- | Parse a CSV file into a list of rows, where each row is a list
-- of Text fields.
csv :: Parser [[T.Text]]
csv = row `sepBy` csvEOL
