{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Table
    ( Row(Row, rCells)
    , Table(Table, tHeaders, tRows)
    , rowCount, columnCount
    , tabularise
    ) where

import Control.Monad
import Control.Monad.Catch
import qualified Data.Text as T

import CSVError

-- | A row in the table.
newtype Row a = Row { rCells :: [a] } deriving (Eq, Show, Foldable)

-- | A table, which contains a header row and zero or more data rows.
data Table a = Table { tHeaders :: Row a   -- ^ The table's header row
                     , tRows    :: [Row a] -- ^ The data rows
                     } deriving (Eq, Show)

-- | The number of rows in the table, not including the header row.
rowCount :: Table a -> Int
rowCount = length . tRows

-- | The number of columns in the table, as defined by the header row.
columnCount :: Table a -> Int
columnCount = length . tHeaders

-- | Take a list of rows and turn them into a table by treating the
-- first row as a header row. Throws a TabularisationError if no
-- header row can be found.
tabularise :: MonadThrow m => [ Row a ] -> m (Table a)
tabularise []     = throwM (TabularisationError "No rows found, so none could be assigned as the table header")
tabularise (x:xs) = return (Table x xs)
