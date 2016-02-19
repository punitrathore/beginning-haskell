module Chap3.MoreModules where

import qualified Data.List as L

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter = filter (\l -> head l == letter) . L.permutations
