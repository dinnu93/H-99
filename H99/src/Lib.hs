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

encode :: Eq t => [t] -> [(Integer,t)]
encode ls = zip pkdLen compList
  where pkdLen = map (toInteger . length) . pack $ ls
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

range :: Integer -> Integer -> [Integer]
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

-- No questions numbered 29 & 30

-- Questions : 31 to 41 on Arthimetic

-- 31) Determine whether a given integer number is prime.

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n
  | n > 0 = not $ any (==0) $ map (mod n) [2..(ceiling . sqrt . fromInteger $ n)] 
  | otherwise = error "Negative integers can't be primes!"

-- 32) Determine the greatest common divisor of two positive integer numbers.

myGcd :: Integer -> Integer -> Integer
myGcd 0 n = n
myGcd n 0 = n
myGcd x y = myGcd (mod high low) low
  where absX = abs x
        absY = abs y
        high = max absX absY
        low = min absX absY

-- 33) Determine whether two positive integer numbers are coprime. Two numbers
-- are coprime if their greatest common divisor equals 1.

coprime :: Integer -> Integer -> Bool
coprime x y = gcd x y == 1

-- 34) Calculate Euler's totient function phi(m).

totient :: Integer -> Integer
totient x = toInteger . length . filter (coprime x) $ [1..(x-1)]

-- 35) Determine the prime factors of a given positive integer.

-- highest power of x that divides n
multiplicity :: Integer -> Integer -> Integer
multiplicity n p
  | mod n p == 0 = 1 + multiplicity quot p 
  | otherwise = 0
  where quot = div n p

primeFactors :: Integer -> [Integer]
primeFactors n = foldr (\p acc -> replicate (fromInteger (multiplicity n p)) p ++ acc) [] primeFactorsOfN 
  where primesBelowHalfN = filter isPrime $[1..(div n 2)]
        primeFactorsOfN = filter (\p -> mod n p == 0) primesBelowHalfN

-- 36) Determine the prime factors of a given positive integer. Construct a
-- list containing the prime factors and their multiplicity.

primeFactorsMult :: Integer -> [(Integer,Integer)]
primeFactorsMult n = map (\(f,s) -> (s,toInteger f)) . encode . primeFactors $ n

-- 37) Calculate Euler's totient function phi(m) (improved).

totientImp :: Integer -> Integer
totientImp n = foldr (\(p,m) acc -> (p-1) * p^(m-1) * acc) 1 primeFactMult
  where primeFactMult = primeFactorsMult n

-- 39) Given a range of integers by its lower and upper limit, construct a list
-- of all prime numbers in that range.

primesR :: Integer -> Integer -> [Integer]
primesR x y = filter isPrime [x..y]
  
-- 40) Goldbach's conjecture. Split any even number into two primes.

goldbach :: Integer -> (Integer,Integer)
goldbach n
  | even n && n > 2 = head . filter (\(f,s) -> f + s == n) $ [(x,y) | x <- primesBelowN, y <- primesBelowN] 
  | otherwise = error "Give even number greater than 2!"
  where primesBelowN = primesR 2 n
        

-- 41) Given a range of integers by its lower and upper limit, print a list of
-- all even numbers and their Goldbach composition.

goldbachList :: Integer -> Integer -> [(Integer, Integer)]
goldbachList x y = map goldbach . filter (\n -> even n && n > 2) $ [x..y]

-- No problems with numbers 42 to 45

-- 46) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2

and2 :: Bool -> Bool -> Bool
and2 = (&&)

or2 :: Bool -> Bool -> Bool
or2 = (||)

nand2 :: Bool -> Bool -> Bool
nand2 x y = not $ and2 x y

nor2 :: Bool -> Bool -> Bool
nor2 x y = not $ or2 x y

equ2 :: Bool -> Bool -> Bool
equ2  = (==)

xor2 :: Bool -> Bool -> Bool
xor2 x y = not $ equ2 x y

impl2 :: Bool -> Bool -> Bool
impl2 x y = or2 (not x) y

-- create the truth table list.

tableList :: (Bool -> Bool -> Bool) -> [[Bool]]
tableList f = map (\[x,y] -> [x,y,f x y]) inputList 
  where inputList = [[x,y] | x <- [True,False], y <- [True,False]]

displayTable :: [[Bool]] -> String
displayTable = unlines . map (\xs -> unwords . map show $ xs) 

table :: (Bool -> Bool -> Bool) -> IO ()
table = putStr . displayTable . tableList  

-- 47) Continue problem P46 by defining and/2, or/2, etc as being operators.

-- Not even a problem :P comes naturally with Haskell. every function can be used as an operator.

-- 48) Generalize problem P46 in such a way that the logical expression may contain any number of logical variables.

inputList :: Integer -> [[Bool]]
inputList 1 = [[True],[False]]
inputList n
  | n > 1 = [x ++ y | x <- inputList (n-1), y <- inputList 1]
  | otherwise = error "Negative numbers not allowed!"
    
tableList' :: Integer -> ([Bool] -> Bool) -> [[Bool]]
tableList' l f =  map (\ls -> ls ++ [f ls]) inList
  where inList = inputList l 

table' :: Integer -> ([Bool] -> Bool) -> IO ()
table' l f = putStr $ displayTable $ tableList' l f  

-- 49) An n-bit Gray code is a sequence of n-bit strings constructed according
-- to certain rules. (Read up gray codes for the rules of construction)

gray :: Integer -> [String]
gray 1 = ["0","1"]
gray n = map ('0':) succGray ++ map ('1':) refSuccGray
  where succGray = gray (n-1)
        reflect = reverse
        refSuccGray = reflect succGray
  
-- 50) Huffman codes. given the map of char and frequency give the huffman code mapping for respective chars.

type Frequency = Integer
type Edge = String
type Code = String
data HFTree =  L {string :: String, frequency :: Frequency} |
               N {string :: String,
                  frequency ::  Frequency,
                  lEdge :: Edge,
                  lHFTree :: HFTree,
                  rEdge :: Edge,
                  rHFTree :: HFTree} deriving (Show)  

addTrees :: HFTree -> HFTree -> HFTree
addTrees l1@(L s1 f1) l2@(L s2 f2) = N (s1 ++ s2) (f1 + f2) "0" l1 "1" l2
addTrees l@(L s1 f1) hft@(N s2 f2 _ _ _ _) = N (s1 ++ s2) (f1 + f2) "0" l "1" hft
addTrees hft@(N s1 f1 _ _ _ _) l@(L s2 f2) = N (s1 ++ s2) (f1 + f2) "0" hft "1" l
addTrees hft1@(N s1 f1 _ _ _ _) hft2@(N s2 f2 _ _ _ _) = N (s1 ++ s2) (f1 + f2) "0" hft1 "1" hft2

foldTree :: [HFTree] -> HFTree
foldTree hftList
  | l == 1 = head hftList
  | l > 1 = foldTree $ headHFTList ++ [addTrees (head tailHFTList) (last tailHFTList)]
  | l < 1 = error "Can't make a Huffman Tree from an empty list!"
  where sortHFT = L.sortBy (\hf1 hf2 -> (frequency hf2) `compare` (frequency hf1)) hftList
        l = length hftList
        headHFTList = take (l-2) sortHFT 
        tailHFTList = drop (l-2) sortHFT 
        
convertFreqMap :: [(Char,Integer)] -> [HFTree]
convertFreqMap = map (\(l,f) -> L [l] f)

makeHFTree :: [(Char,Integer)] -> HFTree
makeHFTree = foldTree . convertFreqMap 

letterCode :: Char -> HFTree -> Code
letterCode c (L s _) = ""
letterCode c hfTree@(N s _ l lTree r rTree )
  | cTree && clTree = l ++ letterCode c lTree
  | cTree && crTree = r ++ letterCode c rTree 
  where cTree = c `elem` (string hfTree)
        clTree = c `elem` (string lTree)
        crTree = c `elem` (string rTree) 

codeMap :: [(Char, Integer)] -> [(Char, Code)]
codeMap ls = map (\(l,f) -> (l,letterCode l lsHFTree)) ls 
  where lsHFTree = makeHFTree ls
  
