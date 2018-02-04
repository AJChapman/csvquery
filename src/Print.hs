module Print
    ( renderTable
    , printTable
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

renderTable :: Table T.Text -> String
renderTable = render . boxTable

printTable :: Table T.Text -> IO ()
printTable = printBox . boxTable
