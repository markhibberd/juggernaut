module Juggernaut.Dependency where

import Data.List

data Module = Mx String [Module] deriving Show

name :: Module -> String
name (Mx n _) = n

dependencies :: Module -> [Module]
dependencies (Mx _ ds) = ds

names :: [Module] -> [String]
names = map name

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
unresolved = unresolved'. dependencies
  where
    unresolved' :: [Module] -> [Module] -> [Module]
    unresolved' ds done = [d | d <- ds, notElem (name d) (names done)]

--    (toexecute, delayed)
layer :: [Module] -> [Module] -> ([Module], [Module])
layer ms done = foldl (\acc m -> 
                        if null (unresolved m done) then 
                          ((fst acc) ++ [m], snd acc) 
                        else (fst acc, (snd acc) ++ [m])
                      ) ([], []) ms
                    
                                

  
  
