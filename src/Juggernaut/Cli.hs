module Juggernaut.Cli where

import Juggernaut.Api
import System.Environment
import System.Exit

juggernaut :: IO ()
juggernaut = getArgs >>= parse >>= putStr . (\x -> juggernaut' (read x))

juggernaut' :: Dependencies -> String
juggernaut' deps = genmakefile deps (\x -> "(cd " ++ x ++ " && ant)")

parse :: [String] -> IO String
parse (x:[]) = readFile x
parse _ = usage >> die

usage :: IO ()
usage =  getProgName >>= putStrLn . (\x -> x ++ " dependency.file")
  
exit :: IO a
exit = exitWith ExitSuccess

die :: IO a
die = exitWith $ ExitFailure 1