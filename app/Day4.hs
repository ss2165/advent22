module Day4 (solve) where

import Data.List.Split (splitOn)

solve :: IO ()
solve = do
    contents <- readFile "inputs/day4.txt"
    print ((length . filter overlaps . map parsepair.lines) contents) 

type Range = (Int, Int)

parseran:: [Char] -> Range
parseran x = parsesplit read (splitOn "-") x

parsepair:: [Char] -> (Range, Range)
parsepair x = parsesplit parseran (splitOn ",") x

parsesplit:: ([Char] -> a) -> ([Char] -> [[Char]]) -> [Char] -> (a, a)
parsesplit reader f x = let xs = f x in
    ((reader.head) xs, (reader.last) xs)

_contains :: Range -> Range -> Bool
_contains r1 (a2, b2) = (inrange a2 r1) && (inrange b2 r1)

contains :: (Range, Range) -> Bool
contains (r1, r2) = (_contains r1 r2) || (_contains r2 r1)

inrange :: Int -> Range -> Bool
inrange x (a, b) = (a <= x) && (x <= b)

_overlaps :: Range -> Range -> Bool
_overlaps r1 (a2, b2) = (inrange a2 r1) || (inrange b2 r1)

overlaps:: (Range, Range) -> Bool
overlaps (r1, r2) = (_overlaps r1 r2) || (_overlaps r2 r1)
