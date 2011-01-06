module Juggernaut.Api where

import Juggernaut.Dependency
import Control.Monad
import Data.Maybe

data Target = T String [String] [String] deriving Show

makefile :: [Target] -> String
makefile ts = join $ map makefile' ts
                      
makefile' :: Target -> String                      
makefile' (T n ds cs) = n ++ ":" ++ (join (map (\d -> " " ++ d) ds)) ++ (join (map (\c -> "\n\t" ++ c) cs)) ++ "\n\n" 

toModules :: [(String, FilePath, String, [(String, FilePath)])] -> [Module]
toModules ls = foldl toModules' [] ls

toModules' :: [Module] -> (String, FilePath, String, [(String, FilePath)])  -> [Module]
toModules' acc (n, f, b, ds) = acc ++ [(Mx n f b (map (\(x, p) -> ((let z = findmodule x acc in if isJust z then fromJust (findmodule x acc) else (error $ "Could not find module: " ++ x)), p)) ds))]

stratas :: [[Module]] -> [Target]
stratas ss = map strata (zip [1..] ss)

strata :: (Int, [Module]) -> Target
strata (i, ds) = T ("strata-" ++ (show i)) (map (\x -> (name x) ++ " ") ds) []

strataup :: [[Module]] -> [Target]
strataup ss =  map strataup' (zip [1..] ss)

strataup' :: (Int, [Module]) -> Target
strataup' (i, _) = T ("strata-" ++ (show i) ++ "-up") ["strata-" ++ (show $ i - 1) ++ "-up"] ["${MAKE} strata-" ++ (show i)]

stratadown :: [[Module]] -> [Target]
stratadown ss =  map stratadown' (zip [1..] ss)

stratadown' :: (Int, [Module]) -> Target
stratadown' (i, _) = T ("strata-" ++ (show i) ++ "-down") ["strata-" ++ (show i)] ["${MAKE} strata-" ++ (show $ i + 1) ++ "-down"]

xups :: [[Module]] -> [Target]
xups ss = join $ map xup (zip [1..] ss)

xup :: (Int, [Module]) -> [Target]
xup (i, ds) = map (xup' i) ds

xup' :: Int -> Module -> Target
xup' i m = let n = name m
            in T (n ++ "-up" ) ["strata-" ++ (show $ i - 1) ++ "-up "] ["${MAKE} " ++ n]

xdowns :: [[Module]] -> [Target]
xdowns ss = join $ map xdown (zip [1..] ss)

xdown :: (Int, [Module]) -> [Target]
xdown (i, ds) = map (xdown' i) ds

xdown' :: Int -> Module -> Target
xdown' i m = let n = name m
              in T (n ++ "-down") [n] ["${MAKE} strata-" ++ (show $ i + 1) ++ "-down "]

workers :: [Module] -> [Target]
workers = map workers'

workers' :: Module -> Target
workers' m = let n = name m
                 ds = dependencies m
                 b = buildcmd m
             in (T n [] ((map (\(d, p) -> "cp " ++ (artifact d) ++ " " ++ n ++ "/" ++ p ++ "/.") ds) ++ [b]))

phony :: [Module] -> Target
phony ms = T ".PHONY" (names ms) []

juggernaut :: [(String, FilePath, String, [(String, FilePath)])] -> String
juggernaut = juggernaut' . toModules 

juggernaut' :: [Module] -> String
juggernaut' = makefile . juggernaut'' 
                     
juggernaut'' :: [Module] -> [Target]                     
juggernaut'' ms = let ss = stratify ms
                 in  [phony ms] ++
                     (workers ms) ++ 
                     (xups ss) ++
                     (xdowns ss) ++
                     (stratas ss) ++
                     [T "strata-0-up" [] []] ++
                     (strataup ss) ++
                     (stratadown ss) ++
                     [T ("strata-" ++ (show $ length ss + 1) ++ "-down") [] []]


