{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-} 

module Chap2.SimpleFunctions where

import Data.Char

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [Char] -> [Char] -> [Char]
lst1 +++ lst2 = if null lst1 then
                  lst2
                else
                  (head lst1):(tail lst1 +++ lst2)

reverse2 :: [Char] -> [Char]
reverse2 lst = if null lst then
                 []
               else
                 reverse2(tail lst)+++[head lst]


maxmin :: [Integer] -> (Integer, Integer)
maxmin lst = let h = head lst
             in if null (tail lst) then
                   (h, h)
                   else (if h > t_max then h else t_max,                              
                         if h < t_min then h else t_min)
                        where t = maxmin (tail lst)
                              t_max = fst t
                              t_min = snd t
                     


                              
data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show

data Gender = Male | Female | Unknown
              deriving Show

data Person = Person String String Gender
              deriving Show


-- data TimeMachine = TMManufacturer
--                    TMModel
--                    TMName
--                    Bool
--                    TMPrice
--                  deriving Show

-- data TMManufacturer = String
-- data TMName = String
-- data TMPrice = Float

clientName :: Client -> String
clientName client = case client of
  GovOrg name -> name
  Company name _ _ _ -> name
  Individual person _ ->
    case person of Person fName lName _ -> fName ++ " " ++  lName

fibonacci :: Integer -> Integer
fibonacci n  =
  case n of
    0 -> 1
    1 -> 1
    _ -> fibonacci(n-1) + fibonacci(n-2)

sorted :: [Integer] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:r@(y:_)) = x < y && sorted(r)

-- maxmin2 :: [Integer] -> (Integer, Integer)
-- maxmin2(x:xs) = (if x > xs_max then x else xs_max
--                ,if x < xs_min then x else xs_min
--                ) where (xs_max, xs_min) = maxmin2 xs

ifibonacci :: Integer -> Maybe Integer
-- ifibonacci n = if n < 0 then
--                   Nothing
--                else case n of
--                      0 -> Just 1
--                      1 -> Just 1
--                      n -> let Just f1 = ifibonacci(n-1)
--                               Just f2 = ifibonacci(n-2)
--                           in Just(f1 + f2)
                 

ifibonacci n | n < 0 = Nothing
ifibonacci 0 = Just 0
ifibonacci 1 = Just 1
ifibonacci n | otherwise = let (Just f1, Just f2) = (ifibonacci(n-1), ifibonacci(n-2))
                             in Just(f1+f2)


binom _ 0          = 1
binom x y | x == y = 1
binom n k          = (binom (n-1) (k-1)) + (binom (n-1) k)


-- ackerman :: Integer -> Integer -> Integer
-- ackerman m n | m == 0 = n + 1
-- ackerman m n | (m > n && n == 0)   = ackerman((m-1) 1)
-- ackerman m n | (m > 0 && n > 0)    = ackerman((m-1), ackerman(m,(n-1)))        


-- unzip :: [(Integer, Integer)] -> ([Integer], [Integer])
-- unzip [] = ()
-- unzip [(a,b)] = ([a,b])
-- unzip [(a,b),(c,d)] = ([a,c],[b,d])

myUnzip :: [(a,b)] -> ([a],[b])
myUnzip [] = ([], [])
myUnzip ((a, b):xs) = (a : (fst rest), b : (snd rest))
    where rest = myUnzip xs

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False


data ClientR = GovOrgR { clientRName :: String}
             | CompanyR { clientRName :: String
                        , companyId :: Integer
                        , person :: PersonR
                        , duty :: String}
             | IndividualR { person :: PersonR}
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show


greet :: ClientR -> String
greet GovOrgR {clientRName = n} = "Welcome, " ++ n
greet CompanyR { person = PersonR {firstName = n}} = "Hello, " ++ n
greet IndividualR { person = PersonR {lastName = n}} = "Dear, " ++ n


greet2 :: ClientR -> String
greet2 GovOrgR { .. } = "Welcome, " ++ clientRName
greet2 CompanyR { person = PersonR { .. } } = "Hello, " ++ firstName
greet2 IndividualR { person = PersonR {lastName = n}} = "Dear, " ++ n

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR{ firstName = initial:rest}) = p{firstName = ((toUpper initial):rest)}
nameInCapitals p@(PersonR{ firstName = ""}) = p
