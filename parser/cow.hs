-- This code _might_ be GHC-specific, but I don't care. So there.
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Here (here)
import System.Environment

-- The multiline string stuff confuses Emacs :(
usage :: String
usage =
  [$here|Cow - the semantic version control system!

usage: cow <command>
where <command> is one of:
  - help

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
execute []           = putStr usage
execute (command:args) = case command of
  "help" -> putStr $ help args