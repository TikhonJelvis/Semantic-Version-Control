-- This code _might_ be GHC-specific, but I don't care. So there.
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Here (here)
import System.Environment

-- The multiline string stuff confuses Emacs :(
usage :: IO ()
usage = putStr [$here|
Cow - the semantic version control system!

usage: cow <command>
where <command> is one of:
  - help

For more information on the commands, use cow help.
|]

help :: [String] -> IO ()
help []             = help ["help"]
help (command:rest) = putStr $ case command of
  "help" -> [$here|
usage: cow help <command>

Prints a help message about the specified command.
|]
  otherwise -> "Cow help: Unknown command!\n"

main :: IO ()
main = getArgs >>= execute

-- TODO: Have this actually do stuff!
execute :: [String] -> IO ()
execute []           = usage
execute (command:args) = case command of
  "help" -> help args