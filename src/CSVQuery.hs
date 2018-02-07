{-# LANGUAGE OverloadedStrings #-}
module CSVQuery
    ( parseCSV
    , parseCSVTable
    , readCSVTableFile
    ) where

import           Control.Monad.Catch     (MonadThrow, throwM)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Text as T
import           System.IO               (withFile, IOMode(ReadMode), hGetContents)
import           Text.Megaparsec         (parse, parseErrorPretty)

import CSVError
import CSVParse
import Table

-- | Parse the given text as a CSV file, and return it as a list of
-- rows, where each row is a list of 'Text' fields.
parseCSV :: MonadThrow m
         => FilePath -- ^ The file that the data came from, for error reporting
         -> T.Text -- ^ The CSV data
         -> m [[T.Text]]
parseCSV f txt = let result = parse csv f txt
                  in case result of
                       Left err -> throwM (InvalidCSV f (parseErrorPretty err))
                       Right rs -> return rs

-- | Parse the given text as a CSV file, and return it as a 'Table'.
parseCSVTable :: MonadThrow m
              => FilePath -- ^ The file that the data came from, for error reporting
              -> T.Text   -- ^ The CSV data
              -> m (Table T.Text)
parseCSVTable f txt = do
    rs <- parseCSV f txt
    let rows = map Row rs
    tabularise rows

-- | Read CSV data from a file and return it as a 'Table'.
readCSVTableFile :: (MonadThrow m, MonadIO m) -- Will do IO, and may throw errors if CSV parsing fails
                 => FilePath -- ^ The file to read
                 -> m (Table T.Text)
readCSVTableFile f = do
    str <- liftIO $ readFile f -- TODO: streaming input
    parseCSVTable f (T.pack str)
