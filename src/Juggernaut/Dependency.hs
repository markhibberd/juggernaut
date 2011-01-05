module Juggernaut.Dependency where

import Data.List

data Module = Mx String FilePath (String -> String) [Module]

name :: Module -> String
name (Mx n _ _ _) = n

dependencies :: Module -> [Module]
dependencies (Mx _ _ _ ds) = ds

names :: [Module] -> [String]
names = map name

stratify :: [Module] -> [[Module]]
stratify = stratify' []
  where
    stratify' :: [Module] -> [Module] -> [[Module]]
    stratify' ms done = let (exe, delay) = (layer ms done)
                        in exe : (stratify' done delay)

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
                    
                                

  
  
