module Juggernaut.Cli where

import Juggernaut.Api

example :: Dependencies
example = [
    ("fred", []),
    ("barney", ["fred"])
  ]

juggernaut :: IO ()
juggernaut = putStrLn $ genmakefile example (\x -> "(cd " ++ x ++ " && ant)")
