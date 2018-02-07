{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit

import qualified Data.Text as T

import CSVQuery
import Filter
import Table

testFile1, testFile2, testFile3 :: FilePath
testFile1 = "test/CSVfile.csv"
testFile2 = "test/CSVfile-fixed.csv"
testFile3 = "test/CSVfile-unix.csv"

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

testsForFile file = do
    filter <- testFilter
    table <- readTestFile file
    filteredTable <- applyFilter filter table
    return (TestList [ TestLabel "testRows" (testRows table)
                     , TestLabel "testCols" (testCols table)
                     , TestLabel "testFilterParses" (testFilterParses filter)
                     , TestLabel "testOneBob" (testOneBob filteredTable)])

main :: IO Counts
main = do
    t1 <- testsForFile testFile1
    t2 <- testsForFile testFile2
    t3 <- testsForFile testFile3
    runTestTT (TestList [t1, t2, t3])
