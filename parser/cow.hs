-- This code _might_ be GHC-specific, but I don't care. So there.
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Here (here)
import Parse
import System.Environment

-- The multiline string stuff confuses Emacs :(
usage :: String
usage =
  [$here|Cow - the semantic version control system!

usage: cow <command>
where <command> is one of:
  - help
  - parse

For more information on the commands, use cow help <command>.
|]

help :: [String] -> String
help []             = help ["help"]
help (command:rest) = case command of
  "help" ->
    [$here|usage: cow help <command>

Prints a help message about the specified command.
|]
  "parse" ->
    [$here|usage: cow parse <file> [<output>]

Parses the given file and writes it to the <output> path. If <output>
is not specified, writes it to <file>.json.
|]
  otherwise -> "Cow help: Unknown command!\n"

main :: IO ()
main = getArgs >>= execute

-- TODO: Have this actually do stuff!
execute :: [String] -> IO ()
execute []             = putStr usage
execute (command:args) = case command of
  "help"  -> putStr $ help args
  "parse" -> if length args == 0 || length args > 2 then putStr $ help ["parse"]
            else do code <- readFile $ args !! 0
                    let out = if length args == 1
                              then (args !! 0) ++ ".json"
                              else args !! 1
                    jsonFile code out
                    putStrLn $ "Done parsing to " ++ out ++ "."

jsonFile :: String -> FilePath -> IO ()
jsonFile code out = writeFile out $ parseToJS code