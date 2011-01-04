module Juggernaut.Api where

import Control.Monad

type Dependency = (String, [String])
type Dependencies = [Dependency]

pull :: Dependency -> String
pull (d, _) = d ++ "-pull:\n\tFIX: fill this in automatically\n\n"

pulls :: Dependencies -> [String]
pulls = map pull

up :: Dependency -> String
up (d, ds) = d ++ "-up: " ++ (join (map (\x -> x ++ "-up") ds)) ++ "\n\t${MAKE} " ++ d ++ "\n\n"

ups :: Dependencies -> [String]
ups = map up

isDownstream :: String -> Dependency -> Bool
isDownstream s (_, ds) = elem s ds

downstream :: String -> Dependencies -> [String]
downstream d ds = map (\x -> fst x) (filter (isDownstream d) ds)

down :: Dependency -> [String] -> String
down (d, _) dn = d ++ "-down: " ++ d ++ "\n\t${MAKE} " ++ (join (map (\x -> x ++ "-down ") dn)) ++ "\n\n"

downs :: Dependencies -> [String]
downs d = map (\x -> down x (downstream (fst x) d)) d

self :: (String -> String) -> Dependency -> String
self f (d, _) = d ++ ": " ++ d ++ "-pull\n\t" ++ (f d) ++ "\n\n"

selfs :: Dependencies -> (String -> String) -> [String]
selfs ds f = map (self f) ds
          
genmakefile :: Dependencies -> (String -> String) -> String
genmakefile ds f = join (genmakefile' ds f)

genmakefile' :: Dependencies -> (String -> String) -> [String]
genmakefile' ds f =  (selfs ds f) ++ (pulls ds) ++ (ups ds) ++ (downs ds)
