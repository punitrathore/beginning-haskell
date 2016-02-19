module Chap3.Lists where

import Prelude hiding (filter, foldr)

filter :: (t -> Bool) -> [t] -> [t]
filter _ [] = []
filter f (x:xs) = if f x then x:(filter f xs) else (filter f xs)

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ initial [] = initial
myfoldr f initial (x:xs) = f x (myfoldr f initial xs)
