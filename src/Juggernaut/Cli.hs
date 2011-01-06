module Juggernaut.Cli where

import Juggernaut.Api
import Juggernaut.Dependency
import System.Environment
import System.Exit
import Control.Monad

fred = Mx "fred" []
barney = Mx "barney" [fred]
bambam = Mx "bambam" [barney]
homer = Mx "homer" [fred, barney]

modules = [fred, barney, bambam, homer]

stratified = stratify modules

yyy s = join $ map (\x -> (name x) ++ ",") s

xxx ss = join $ map (\x -> (yyy x) ++ "\n" ) ss

stratas :: [[Module]] -> [String]
stratas ss = map strata (zip [1..] ss)

strata :: (Int, [Module]) -> String
strata (i, ds) = "strata-" ++ (show i) ++ ": " ++ (join (map (\x -> (name x) ++ " ") ds)) ++ " | strata-" ++ (show $ i - 1)

strataup :: [[Module]] -> [String]
strataup ss =  map strataup' (zip [1..] ss)

strataup' :: (Int, [Module]) -> String
strataup' (i, _) = "strata-" ++ (show i) ++ "-up: strata-" ++ (show i) ++ " strata-" ++ (show $ i - 1) ++ "-up" 

stratadown :: [[Module]] -> [String]
stratadown ss =  map stratadown' (zip [1..] ss)

stratadown' :: (Int, [Module]) -> String
stratadown' (i, _) = "strata-" ++ (show i) ++ "-down: strata-" ++ (show i) ++ " strata-" ++ (show $ i + 1) ++ "-down" 

xups :: [[Module]] -> [String]
xups ss = map xup (zip [1..] ss)

xup :: (Int, [Module]) -> String
xup (i, ds) = join $ map (\d -> (xup' i d) ++ "\n") ds

xup' :: Int -> Module -> String
xup' i m = (name m) ++ "-up: strata-" ++ (show $ i - 1) ++ "-up " ++ (name m)

xdowns :: [[Module]] -> [String]
xdowns ss = map xdown (zip [1..] ss)

xdown :: (Int, [Module]) -> String
xdown (i, ds) = join $ map (\d -> (xdown' i d) ++ "\n") ds

xdown' :: Int -> Module -> String
xdown' i m = (name m) ++ "-down: strata-" ++ (show $ i + 1) ++ "-down " ++ (name m)


juggernaut :: IO ()
juggernaut = putStrLn $ join $ map (\x -> x ++ "\n") (["strata-0:", "strata-0-down:", "strata-0-up:"] ++ (stratas stratified) ++ (xups stratified) ++ (xdowns stratified) ++ (strataup stratified) ++ (stratadown stratified))

juggernautx :: IO ()
juggernautx = getArgs >>= parse >>= putStr . (\x -> juggernaut' (read x))

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