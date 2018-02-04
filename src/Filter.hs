module Filter
    ( Filter
    , parseFilter
    , applyFilter
    ) where

import Control.Monad (void)
import Control.Monad.Catch
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Table

-- | Table filtering, based on column values.
-- Example filters: "*"         : Don't filter
--                  "Name=Dave" : Show only rows whose 'Name' column contains the value 'Dave'.
data Filter a = NullFilter
              | Constraint { cColumn :: a, cValue :: a }
             -- | And { aLeft :: Filter a, aRight :: Filter a }
             -- | Or { oLeft :: Filter a, oRight :: Filter a }
             deriving (Eq, Show)

-- | Apply the given 'Filter' to the table. The resultant table will
-- only contains rows which match the filter.
applyFilter :: (MonadThrow m, Eq a, Show a) => Filter a -> Table a -> m (Table a)
applyFilter NullFilter t = return t
applyFilter (Constraint col val) t = do
    i <- columnIndex col t
    let rowFilter = (\r -> ((rCells r) !! i) == val) in
        return (filterTable rowFilter t)

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

nullFilterParser :: Parser (Filter a)
nullFilterParser = do
    void (lexeme (char '*')) <?> "asterisk symbol"
    return NullFilter

equals :: Parser ()
equals = void (lexeme (char '=')) <?> "equals symbol"

field :: Parser T.Text
field = lexeme (takeWhileP (Just "field text") (not . (== '=')))

constraintParser :: Parser (Filter T.Text)
constraintParser = do
    name <- field
    equals
    val <- field
    return (Constraint name val)

filterParser :: Parser (Filter T.Text)
filterParser = nullFilterParser <|> constraintParser

data FilterParseError = InvalidFilter T.Text String
instance Exception FilterParseError
instance Show FilterParseError where
    show (InvalidFilter f msg) = "Invalid filter '" ++ T.unpack f ++ "': " ++ msg

-- | Parse a filter such as "Name=Dave" and produce a 'Filter' which can be applied with 'applyFilter'.
parseFilter :: MonadThrow m => T.Text -> m (Filter T.Text)
parseFilter t =
    let result = parse filterParser "" t
     in case result of
          Left err -> throwM (InvalidFilter t (parseErrorPretty err))
          Right f  -> return f
