{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Table
    ( Row(Row, rCells)
    , Table(Table, tHeaders, tRows)
    , tabularise
    , rowCount, columnCount, columnIndex, rowLength
    , filterTable
    , getRow, getColumn
    ) where

import Control.Monad
import Control.Monad.Catch
import qualified Data.Text as T
import Data.List
import Data.Typeable

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

-- | The number of cells in the row.
rowLength :: Row a -> Int
rowLength = length . rCells

-- | Error type for table operations
data TableError = NoRowsError
                | ColumnCountError Int
                | UnknownColumnError String
                | RowIndexOutOfBoundsError Int Int
                deriving (Eq, Ord, Typeable)

-- | How to display table errors
instance Show TableError where
    show NoRowsError
      = "No rows found, so none could be assigned as the table header."
    show (ColumnCountError n)
      = "Rows do not all have the same number of columns as the header, which has " ++ show n ++ "."
    show (UnknownColumnError h)
      = "Could not find a column labeled " ++ h ++ "."
    show (RowIndexOutOfBoundsError i n)
      = "Could not find row " ++ show i ++ ", as there are only " ++ show n ++ " rows."

instance Exception TableError

-- | Take a list of rows and turn them into a table by treating the first row
-- as a header row. Throws a TableError if no header row can be found, or if
-- the number of items in any row does not match the number of columns in the
-- header.
tabularise :: MonadThrow m => [ Row a ] -> m (Table a)
tabularise []     = throwM NoRowsError
tabularise (x:xs) =
    let n = length x in
        if (all (lengthIs n) xs)
           then return (Table x xs)
           else throwM (ColumnCountError n)
       where lengthIs :: Int -> Row a -> Bool
             lengthIs n r = n == (rowLength r)

-- | Filter the table to contain only rows matching the given predicate.
filterTable :: (Row a -> Bool) -> Table a -> Table a
filterTable f (Table h rs) = Table h (filter f rs)

-- | Return the ith row of the table, not including the table header. So the
-- first row below the table header is row 0. Throws a RowIndexOutOfBoundsError
-- if the row doesn't exist.
getRow :: MonadThrow m => Int -> Table a -> m (Row a)
getRow i (Table _ rs) =
    let n = length rs
     in if (i >= n)
           then throwM (RowIndexOutOfBoundsError i n)
           else return (rs !! i)

-- | Return the index of the column with the given header, or throw an
-- UnknownColumnError.  If more than one column with this header exists then
-- only the first will be returned.
columnIndex :: (MonadThrow m, Eq a, Show a)
            => a  -- ^ The column header. Label must match exactly (case-sensitive).
            -> Table a -> m Int
columnIndex c (Table h _) =
    let mi = elemIndex c (rCells h)
     in case mi of
          Nothing -> throwM (UnknownColumnError (show c))
          Just i -> return i

-- | Return the list of fields in the column with the given header, or throw an
-- UnknownColumnError.  If more than one column with this header exists then
-- only the first will be returned.
getColumn :: (MonadThrow m, Eq a, Show a)
          => a -- ^ The column header. Label must match exactly (case-sensitive).
          -> Table a -> m [a]
getColumn c t@(Table _ rs) = do
    i <- columnIndex c t
    return (map ((!! i) . rCells) rs)
