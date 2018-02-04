module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Text as T

import CSVQuery
import Filter
import Print
import Table

data Options = Options
    { csvfile :: FilePath
    , filter  :: String
    , field   :: String }

options :: Parser Options
options = Options
            <$> strArgument
                (metavar "FILE"
                <> help "The CSV file to read")
            <*> strOption
                (long "filter"
                <> metavar "FILTER"
                <> value "*"
                <> help "An optional row filter, e.g. \"Name=Dave\". If not specified then all rows will be displayed.")
            <*> strOption
                (long "field"
                <> metavar "FIELD"
                <> value ""
                <> help "Select a single field to display. If not specified then the data will be displayed in a table.")

main :: IO ()
main = run =<< execParser opts
    where opts = info (options <**> helper)
                      (fullDesc
                         <> progDesc "Read FILE as a CSV file and print its contents or answer queries about it"
                         <> header "csvquery - a CSV file querier")

run :: Options -> IO ()
run (Options file filterString field) = do
    table  <- readCSVTableFile file
    filter <- parseFilter (T.pack filterString)
    table' <- applyFilter filter table
    if field == ""
       then printTable table'
       else printField (T.pack field) table'
