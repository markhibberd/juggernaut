module Juggernaut.Dependency where

import Data.Maybe 

data Module = Mx String FilePath String [(Module, FilePath)] deriving Show

name :: Module -> String
name (Mx n _ _ _) = n

names :: [Module] -> [String]
names = map name

named :: [(Module, FilePath)] -> [String]
named ds = map name (map (\(m, _) -> m) ds)

dependencies :: Module -> [(Module, FilePath)]
dependencies (Mx _ _ _ ds) = ds

dependenciesd :: Module -> [Module]
dependenciesd (Mx _ _ _ ds) = map (\(m, _) -> m) ds

buildcmd :: Module -> String
buildcmd (Mx _ _ b _) = b
  
artifact :: Module -> FilePath
artifact (Mx _ a _ _) = a

findmodule :: String -> [Module] -> Maybe Module
findmodule n ms = listToMaybe $ filter (\m -> (name m) == n) ms

upstream :: String -> [Module] -> [[Module]]
upstream n ms =  takeWhile (\ls -> notElem n (names ls)) (stratify ms)

downstream :: String -> [Module] -> [[Module]]
downstream n ms = tail $ dropWhile (\ls -> notElem n (names ls)) (stratify ms)

stratify :: [Module] -> [[Module]]
stratify = stratify' []
  where
    stratify' :: [Module] -> [Module] -> [[Module]]
    stratify' _ [] = []
    stratify' done ms = let (exe, delay) = (layer ms done)
                        in exe : (stratify' (exe ++ done) delay)
                           
unresolved :: Module -> [Module] -> [Module]
unresolved = unresolved'. dependenciesd
  where
    unresolved' :: [Module] -> [Module] -> [Module]
    unresolved' ds done = filter (\d -> notElem (name d) (names done)) ds

layer :: [Module] -> [Module] -> ([Module], [Module])
layer ms done = foldl (\(exe, delay) m -> 
                        if null (unresolved m done) then 
                          (exe ++ [m], delay) 
                        else 
                          (exe, delay ++ [m])
                      ) ([], []) ms
                    
                                

  
  
