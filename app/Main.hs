module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import CSVQuery
import Print

data Options = Options
    { csvfile :: FilePath
    }

options :: Parser Options
options = Options
            <$> strArgument
                ( metavar "FILE"
                <> help "The CSV file to read" )

main :: IO ()
main = run =<< execParser opts
    where opts = info (options <**> helper)
                      (fullDesc
                         <> progDesc "Read FILE as a CSV file and print its contents or answer queries about it"
                         <> header "csvquery - a CSV file querier")

run :: Options -> IO ()
run (Options f) = do
    table <- readCSVTableFile f
    printTable table
