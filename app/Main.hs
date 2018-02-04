module Main where

import CSVQuery

main :: IO ()
main = do
    table <- readCSVTableFile "test/CSVfile.csv"
    print $ show table
