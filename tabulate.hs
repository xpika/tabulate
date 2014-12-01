{-# Language OverloadedStrings #-}

import Control.Arrow      ((&&&), (>>>))
import Prelude            (unlines,($),Bool (..),flip, map, uncurry, zipWith, maximum, id, (==), (>>=), putStr, IO, String , (.) )
import Data.List          (transpose,intersperse)
import Data.Text          (Text, lines, intercalate, length, splitOn, justifyLeft, unlines, stripEnd, pack,unpack,concat)
import Data.Text.IO       (interact)
import System.Environment (getArgs)
import Text.Regex (splitRegex, mkRegex)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run []         = run ["\t"]
run [sep]      = exec False (pack sep)
run ["-r",sep] = exec True  (pack sep)
run _          = putStr $ Prelude.unlines [ "Usage: tabulate [delimiter]"
                                          , "       tabulate -r regexp"
                                           ]

exec :: Bool -> Text -> IO ()
exec isRegexp sep = interact tabulate
  where
    tabulate :: Text ->  Text
    tabulate  = rows >>> columns
                     >>> (lengths &&& id)
                     >>> uncurry (zipWith uncolumns)
                     >>> unrows

    rows            = if isRegexp then lines >>> map (splitOn sep)
                                  else lines >>> map (splitOn sep >>> Data.List.intersperse sep)
    columns         = transpose
    lengths         = map (map length >>> maximum)
    uncolumns width = map (justifyLeft width ' ')
    unrows          = transpose >>> map (concat >>> stripEnd) >>> Data.Text.unlines
