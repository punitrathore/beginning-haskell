{-# LANGUAGE TemplateHaskell #-}

module Chap3.Functions where

data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String,
                          person :: Person, duty :: String}
              | Individual { clientId :: i, person :: Person}
              deriving Show

data Person = Person { firstName :: String, lastName :: String }
            deriving Show

mymap :: (t -> t) -> [t] -> [t]
mymap _ [] = []
mymap f (x:xs) = (f x):(mymap f xs)

-- Anonymous functions
multiplybyN :: Integer -> (Integer -> Integer)
multiplybyN n = \x -> n*x

-- map (multiplybyN 5) [1,2,3]

-- Exercise 3.2
filterOnes :: [Integer] -> [Integer]
filterOnes lst = filter (\x -> x == 1) lst

filterANumber :: Integer -> [Integer] -> [Integer]
filterANumber n lst = filter (\x -> x == n) lst

filterNot :: (Integer -> Bool) -> [Integer] -> [Integer]
filterNot f lst = filter (\x -> not(f x)) lst

isGov :: Client t -> Bool
isGov c = case c of
            (GovOrg _ _) -> True
            _ -> False

filterGovs :: [Client t] -> [Client t]
filterGovs clients = filter isGov clients


-- Partial functions
-- duplicateOdds lst = map (*2) $ filter odd lst
duplicateOdds :: [Integer] -> [Integer]
duplicateOdds = map(*2).filter odd


-- Currying and Uncurrying
-- curry :: ((a,b) -> c) -> a -> b -> c
-- curry f = \x y -> f(x,y)
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry f = \(x,y) -> f x y

-- max 3 2
-- (uncurry max) (3, 2)

(***) :: (a -> b) -> (c -> d) -> ((a,c) -> (b,d))
f***g = \(x, y) -> (f x, g y)

duplicate x = (x,x)

formula1 :: Integer -> Integer
formula1 = uncurry (+) . ( ((*7) . (+2)) *** (*3) ) . duplicate

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \x y -> f y x
