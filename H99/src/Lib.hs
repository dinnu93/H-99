module Lib
    ( someFunc
    ) where

import qualified Data.List as L

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Questions : 1 to 10 on Lists

-- 1) Find the Last element of a List
myLast :: [t] -> t
myLast [] = error "No elements in an empty list to find the last one!"
myLast [x] = x
myLast (x:xs) = myLast xs

-- 2) Find last but one element of a List
myButLast :: [t] -> t
myButLast [] = error "No elements in an empty list to find the last one!"
myButLast [x] = error "No last but one element in a singleton list!"
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs

-- 3) Find the kth element in a List
elementAt :: [t] -> Int -> t
elementAt [] n = error "Empty list error!"
elementAt (x:xs) 1 = x
elementAt ls@(x:xs) n
  | n < 1 = error "Positive whole numbers only!" 
  | l >= n = elementAt xs (n-1)
  | otherwise = error "List index exceeded!"
  where l = length ls 

-- 4) Find the number of elements of a list
myLength :: [t] -> Int
myLength = foldr (\x acc -> 1 + acc) 0 

-- 5) Reverse a list
myReverse :: [t] -> [t]
myReverse = foldl (flip (:)) []

-- 6) Find out whether a list is a palindrome
isPalindrome :: Eq t => [t] -> Bool
isPalindrome ls = ls == (myReverse ls) 

-- 7) Flatten a nested list structure :: We have to define a new data type as
--    lists in Haskell are homogenous

data NestedList a = Elem a |List [NestedList a] deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List ls) = foldr (++) [] . map flatten $ ls 

-- 8) Eliminate consecutive duplicates of list elements

compress :: Eq t => [t] -> [t]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
  | x == y = compress (y:xs)
  | otherwise = [x] ++ compress (y:xs)

-- 9) Pack consecutive duplicates of list elements into sublists

-- takes 'aaaabbcc' and gives 'aaaa' as output
headPack :: Eq t => [t] -> [t]
headPack [] = []
headPack [x] = [x] 
headPack (x:y:xs)
  | x == y = [x] ++ headPack (y:xs)
  | otherwise = [x]

tailPack :: Eq t => [t] -> [t]
tailPack ls = ls L.\\ (headPack ls)

pack :: Eq t => [t] -> [[t]]
pack [] = []
pack ls = [headPack ls] ++ pack (tailPack ls) 

-- 10) Run-length encoding of a list

encode :: Eq t => [t] -> [(Int,t)]
encode ls = zip pkdLen compList
  where pkdLen = map length . pack $ ls
        compList = compress ls

-- 11) Modified run-length encoding to give no length for non repetetive
-- elements

data Pair a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq t => [t] -> [Pair t]
encodeModified ls = zipWith (\l e -> if l == 1 then Single e else Multiple l e) pkdLen compList
  where pkdLen = map length . pack $ ls
        compList = compress ls

-- 12) decode the result generated in Problem 11

decodePair :: Pair t -> [t]
decodePair (Single x) = [x]
decodePair (Multiple n x) = replicate n x

decodeModified :: [Pair t] -> [t]
decodeModified = foldr (\e acc -> decodePair e ++ acc) [] 

-- 13)

