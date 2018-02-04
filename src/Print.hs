module Print
    ( renderTable
    , printTable
    , printField
    ) where

import Data.List
import qualified Data.Text as T
import Text.PrettyPrint.Boxes

import Table

boxTable :: Table T.Text -> Box
boxTable t =
    let t'   = fmap (text . T.unpack) t
        cols = tableAsColumns t'
     in hsep 2 left (map (vcat left) cols)

-- | Render the table to its ascii form
renderTable :: Table T.Text -> String
renderTable = render . boxTable

-- | Print the table in its ascii form
printTable :: Table T.Text -> IO ()
printTable = printBox . boxTable

boxColumn :: [T.Text] -> Box
boxColumn = vcat left . (fmap (text . T.unpack))

-- | Print the column with the given heading
printField :: T.Text -> Table T.Text -> IO ()
printField c t = do
    col <- getColumn c t
    printBox (boxColumn col)
