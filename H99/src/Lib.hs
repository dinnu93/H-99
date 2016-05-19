module Lib
    ( someFunc
    ) where

import qualified Data.List as L
import qualified System.Random as R

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Questions : 1 to 30 on Lists

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

-- 13) Run-length encoding of a list (direct solution)

encodeDirect :: Eq t => [t] -> [Pair t]
encodeDirect = encodeModified

-- 14) Duplicate the elements of a list.

dupli :: [t] -> [t]
dupli = foldr (\e acc -> [e, e] ++ acc) [] 

-- 15) Replicate the elements of a list a given number of times.

repli :: [t] -> Int -> [t]
repli ls n = foldr (\e acc -> replicate n e ++ acc) [] ls

-- 16) Drop every N'th element from a list.

dropEvery :: [t] -> Int -> [t]
dropEvery ls n = foldr (\(i,e) acc -> if i`mod`n == 0 then acc else e:acc) [] indexedList
  where indexedList = zip [1..(length ls)] ls 

-- 17) Split a list into two parts; the length of the first part is given.

split :: [t] -> Int -> ([t],[t])
split ls n = (take n ls, drop n ls)

-- 18) Extract a slice from a list.

slice :: [t] -> Int -> Int -> [t]
slice ls x y = drop (x-1) . take y $ ls 

-- 19) Rotate a list N places to the left.

rotate :: [t] -> Int -> [t]
rotate ls n = drop modN ls ++ take modN ls
  where modN = n `mod` (length ls)

-- 20) Remove the K'th element from a list.

removeAt :: Int -> [t] -> (t,[t])
removeAt n ls = (ls !! (n-1), take (n-1) ls ++ drop n ls) 

-- 21) Insert an element at a given position into a list.

insertAt :: t -> [t] -> Int -> [t]
insertAt e ls n = f ++ (e : s)
  where splitLs@(f,s) = split ls (n-1)   

-- 22) Create a list containing all integers within a given range.

range :: Int -> Int -> [Int]
range x y
  | x == y = [x]
  | otherwise = x : range ((if x<y then succ else pred) x) y
 
-- 23) 24) 25) Leaving the Random problems for future :P

-- 26) Generate the combinations of K distinct objects chosen from the N elements of a list

combinations :: Int -> [t] -> [[t]]
combinations n ls
  | n == l = [ls]
  | n == 1 = map (\x -> [x]) ls
  | n < l = foldr (++) [] $ map (\xs -> map ((head xs):) $ combinations (n-1) (tail xs)) $ map (`drop` ls) [0..(l-n)]
  | otherwise = error "Invalid"
  where l = length ls

-- 27) Group problem looks tougher so I'll come back to it later.
        
-- 28) Sorting a list of lists according to length of sublists.

-- a) The objective is to sort the elements of this list according to their length.

lsort :: Ord t => [[t]] -> [[t]]
lsort ls = map snd . L.sort $ indexLs
  where indexLs = map (\e -> (length e, e)) ls
        
-- b) objective is to sort the elements of this list according to their length
-- frequency; i.e., in the default, where sorting is done ascendingly, lists
-- with rare lengths are placed first, others with a more frequent length come
-- later.

lfsort :: Ord t => [[t]] -> [[t]]
lfsort ls = map (\(f,e) -> e) . L.sort $ freqLs
   where lenLs = map length ls
         freqLookup = map (\(f,s) -> (s,f)) . encode . L.sort $ lenLs
         freqLs = map (\e -> (lookup (length e) freqLookup,e)) $ ls
