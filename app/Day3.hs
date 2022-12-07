module Day3 (solve) where
import Data.Char (ord)
import Data.List (find)
import Data.Set (fromList, Set, member, intersection, toList)


solve :: IO ()
solve = do
    contents <- readFile "inputs/day3.txt"
    -- let scores = (map score_round.rounds) contents

    -- print ((sum . map priority . map mistake . lines) contents)
    print ((sum . map priority .join3 . map fromList . lines) contents) 

overlap:: Ord a => Set a -> [a] -> a
overlap set lst = case find (\x -> member x set) lst of
    Just a -> a
    Nothing -> error "not found"



split :: Ord a => [a] -> (Set a, [a])
split str = let n = div (length str) 2 in 
    let (st, end) = splitAt n str in
        (fromList st, end)


mistake :: Ord a => [a] -> a
mistake lst = pairapply overlap (split lst) 


pairapply :: (a -> b -> c) -> (a, b) -> c
pairapply f (a, b) = f a b

priority:: Char -> Int
priority x = case ord x of
    o | o < 97 -> o - 38
    o -> o - 96


-- find3:: Eq a => [a] -> a
-- find3 (a:b:c:xs) = if a == b && b == c then a else find3 xs
-- find3 _ = error "can't find"

join3:: Ord a => [Set a] -> [a]
join3 [] = []
join3 (a:b:c:xs) =  (inters3 a b c):(join3 xs)
join3 _ = error "not multiple of 3"

inters3::Ord a => Set a -> Set a -> Set a -> a
inters3 a b c = (head.toList) ( a `intersection` b `intersection` c)