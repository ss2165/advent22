module Day1 (solve) where
import Data.List.Split (splitOn)
import Data.List (sort)

solve :: IO ()
solve = do
    contents <- readFile "inputs/day1.txt"
    print (maximum [sum [read i | i <- lines gang] | gang <- splitOn "\n\n" contents])
    print ((mysum . top3 . myrev . sort . mymap mysum . spt) contents)

spt :: [Char] -> [[Int]]
spt = mymap (mymap read . lines) . splitOn "\n\n"

top3 :: [a] -> [a]
top3 (a:b:c:_) = [a, b, c]
top3 _ = []

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = (f x):(mymap f xs)

mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xs) = x + mysum xs

myrev :: [a] -> [a]
myrev l = rev l [] where
    rev [] a = a
    rev (x:xs) a = rev xs (x:a)