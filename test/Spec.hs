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

testFilterParses filter =
    TestCase (assertEqual
                "parse Name=Bob filter"
                (Constraint "Name" "Bob")
                filter)

testOneBob table =
    TestCase (assertEqual
                 "filtering with Name=Bob gives us a single row"
                 (rowCount table) 1)

testDavesPhoneNumber numbers =
    TestCase (assertEqual
                "check that Dave's phone number is what we expect"
                ["0400 001 123"]
                numbers)

testsForFile file = do
    filterBob <- parseFilter "Name=Bob"
    filterDave <- parseFilter "Name=Dave"
    table <- readTestFile file
    filteredTableBob <- applyFilter filterBob table
    filteredTableDave <- applyFilter filterDave table
    daveNumbers <- getColumn "Phone" filteredTableDave
    return (TestList [ TestLabel "testRows" (testRows table)
                     , TestLabel "testCols" (testCols table)
                     , TestLabel "testFilterParses" (testFilterParses filterBob)
                     , TestLabel "testOneBob" (testOneBob filteredTableBob)
                     , TestLabel "testDaveNumber" (testDavesPhoneNumber daveNumbers)])

main :: IO Counts
main = do
    t1 <- testsForFile testFile1
    t2 <- testsForFile testFile2
    t3 <- testsForFile testFile3
    runTestTT (TestList [t1, t2, t3])
