module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Text as T

import CSVQuery
import Filter
import Print
import Table

data Options = Options
    { csvfile :: FilePath -- ^ The CSV file to read
    , filter  :: String   -- ^ A filter to apply
    , field   :: String   -- ^ A field to display
    , count   :: Bool     -- ^ Whether to simply display a count of matched rows
    }

-- | A parser for the command-line options
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
                <> help "Select a single field to display. If not specified then the data will be displayed in a table. Ignored when the --count flag is used.")
            <*> switch
                (long "count"
                <> short 'c'
                <> help "Specify to print the number of rows in the file which match the current filter.")

-- | Run with the given options.
run :: Options -> IO ()
run (Options file filterString field count) = do
    table  <- readCSVTableFile file
    filter <- parseFilter (T.pack filterString)
    table' <- applyFilter filter table
    if count
       then print (rowCount table')
       else if field == ""
               then printTable table'
               else printField (T.pack field) table'

-- | The main function. Will take options from the command line.
main :: IO ()
main = run =<< execParser opts
    where opts = info (options <**> helper)
                      (fullDesc
                         <> progDesc "Read FILE as a CSV file and print its contents or answer queries about it"
                         <> header "csvquery - a CSV file querier")

