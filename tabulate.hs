{-# Language OverloadedStrings #-}

import Control.Arrow      ((&&&), (>>>))
import Prelude            (filter,not,unlines,($),Bool (..),flip, map, uncurry, zipWith, maximum, id, (==), (>>=), putStr, IO, String , (.) )
import Data.List          (transpose,intersperse,filter)
import Data.Text          (null,Text, lines, intercalate, length, splitOn, justifyLeft, unlines, stripEnd, pack,unpack,concat)
import Data.Text.IO       (interact)
import System.Environment (getArgs)
import Text.Regex (splitRegex, mkRegex)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run []         = run ["\t"]
run [sep]      = exec False (pack sep)
run ["-m",sep] = exec True  (pack sep)
run ["--many",sep] = exec True (pack sep)
run _          = putStr $ Prelude.unlines [ "Usage: tabulate [-m --many] [delimiter]" ]

exec :: Bool -> Text -> IO ()
exec splitMany sep = interact tabulate
  where
    tabulate :: Text ->  Text
    tabulate  = rows >>> columns
                     >>> (lengths &&& id)
                     >>> uncurry (zipWith uncolumns)
                     >>> unrows

    rows            = lines >>> map (splitOn sep >>> joinCommon >>> Data.List.intersperse sep)
    joinCommon      = if splitMany then filter (not . null) else id
    columns         = transpose
    lengths         = map (map length >>> maximum)
    uncolumns width = map (justifyLeft width ' ')
    unrows          = transpose >>> map (concat >>> stripEnd) >>> Data.Text.unlines
