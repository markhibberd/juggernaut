module Juggernaut.Dependency where

data Module = Mx String [Module] deriving Show

name :: Module -> String
name (Mx n _) = n

names :: [Module] -> [String]
names = map name

dependencies :: Module -> [Module]
dependencies (Mx _ ds) = ds

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
    unresolved' ds done = filter (\d -> notElem (name d) (names done)) ds

layer :: [Module] -> [Module] -> ([Module], [Module])
layer ms done = foldl (\(exe, delay) m -> 
                        if null (unresolved m done) then 
                          (exe ++ [m], delay) 
                        else 
                          (exe, delay ++ [m])
                      ) ([], []) ms
                    
                                

  
  
