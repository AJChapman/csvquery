module CSVError
    ( CSVError(InvalidCSV, TabularisationError)
    ) where

import Control.Exception
import Data.Typeable
import Text.Megaparsec (ShowErrorComponent, showErrorComponent)

data CSVError = InvalidCSV FilePath String
              | TabularisationError String
              deriving (Eq, Ord, Typeable)

-- Tell Haskell how to print our errors.
instance Show CSVError where
    show (InvalidCSV file msg) = "Error when trying to parse file '" ++ file ++ "' as csv: " ++ msg
    show (TabularisationError msg) = "Unable to tabularise data: " ++ msg

-- Make CSVError throwable.
instance Exception CSVError

-- Allow use of CSVError with Megaparsec.
instance ShowErrorComponent CSVError where
    showErrorComponent = show
