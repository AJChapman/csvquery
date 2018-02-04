module Query
    ( Query(Constraint)
    , query
    ) where

import Control.Monad.Catch

import Table

-- Example query: '"Name" = "Dave"
data Query a = Constraint { cColumn :: a, cValue :: a }
             -- | And { aLeft :: Query a, aRight :: Query a }
             -- | Or { oLeft :: Query a, oRight :: Query a }
             deriving (Eq, Show)

query :: (MonadThrow m, Eq a, Show a) => Query a -> Table a -> m (Table a)
query q t = do
    query <- (buildQuery q t)
    return (filterTable query t)
        where buildQuery :: (MonadThrow m, Eq a, Show a) => Query a -> Table a -> m (Row a -> Bool)
              buildQuery (Constraint col val) t = do
                  i <- columnIndex col t
                  return (\r -> ((rCells r) !! i) == val)
