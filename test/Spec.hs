{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit

import qualified Data.Text as T

import CSVQuery
import Filter
import Table

testFile :: FilePath
testFile = "test/CSVfile.csv"

readTestFile :: FilePath -> IO (Table T.Text)
readTestFile f = readCSVTableFile f

testRows table = TestCase (assertEqual "four rows in table" 4 (rowCount table))
testCols table = TestCase (assertEqual "three columns in table" 3 (columnCount table))

testFilter :: IO (Filter T.Text)
testFilter = parseFilter "Name=Bob"

testFilterParses filter =
    TestCase (assertEqual
                "parse Name=Bob filter"
                (Constraint "Name" "Bob")
                filter)

testOneBob table =
    TestCase (assertEqual
                 "filtering with Name=Bob gives us a single row"
                 (rowCount table) 1)

tests table filter filteredTable = TestList
    [ TestLabel "testRows" (testRows table)
    , TestLabel "testCols" (testCols table)
    , TestLabel "testFilterParses" (testFilterParses filter)
    , TestLabel "testOneBob" (testOneBob filteredTable)
    ]

main :: IO Counts
main = do
    table <- readTestFile testFile
    filter <- testFilter
    filteredTable <- applyFilter filter table
    runTestTT (tests table filter filteredTable)
