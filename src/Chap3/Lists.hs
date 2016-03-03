module Chap3.Lists where

import Prelude hiding (filter, foldr)

data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String,
                          person :: Person, duty :: String}
              | Individual { clientId :: i, person :: Person}
              deriving Show

data Person = Person { firstName :: String, lastName :: String }
            deriving Show

filter :: (t -> Bool) -> [t] -> [t]
filter _ [] = []
filter f (x:xs) = if f x then x:(filter f xs) else (filter f xs)

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ initial [] = initial
myfoldr f initial (x:xs) = f x (myfoldr f initial xs)


myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl _ initial [] = initial
myfoldl f initial (x:xs) = myfoldl f (f initial x) xs

---------------- Exercise 3.3-----------------------------
myproduct :: [Integer] -> Integer
myproduct [] = 1
myproduct (x:xs) = x * (myproduct xs)

myall :: [Bool] -> Bool
myall [] = True
myall (x:xs) = x && (myall xs)

myallf :: [Bool] -> Bool
myallf x = myfoldr (&&) True x

myproductf :: [Integer] -> Integer
myproductf x = myfoldl (*) 1 x

-- minimumBy :: (a -> a) -> [a] -> a
minimumBy f l = f $ foldr1 min $ map f l
-------------------------------------------------------------

isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _ = False

checkIndividualAnalytics :: [Client a] -> (Bool, Bool)
checkIndividualAnalytics cs = (any isIndividual cs, not $ all isIndividual cs)


