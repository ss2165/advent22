{-# LANGUAGE OverloadedStrings #-}

module Day5 (solve) where
import Data.Text(pack, unpack, replace, Text)
import Data.List.Split (splitOn)
import Data.List (transpose)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

solve :: IO ()
solve = do
    contents <- readFile "inputs/day5.txt"
    let filtered = (splitOn "\n\n" . remove_many  ["[", "]", "move", "from", "to"]) contents
    let stack_str = head filtered
    let instr_str = last filtered
    let stack_ar = (map mhead . init . map (splitOn " ") . lines) stack_str
    let crates = (map (filter (/= '-')) . transpose) stack_ar
    let crate_map = Map.fromList $ enumerate crates
    let instrs = (map (read_move .map readInt . splitOn "  " . tail).lines) instr_str
    print (map head . Map.elems $ apply_moves instrs crate_map)

type Move = (Int, Int, Int)
type Stack = [Char]
type StackMap = Map.Map Int Stack

read_move :: [c] -> (c, c, c)
read_move  x = case x of 
    (a:b:c:[]) -> (a, b, c)
    _ -> error "wrong length"

readInt :: [Char] -> Int
readInt = read

enumerate :: [a] -> [(Int, a)]
enumerate x = zip [1..length x] x

_replace :: Text -> Text -> String -> String
_replace s r  = unpack . replace s r . pack

remove :: Text -> String -> String
remove s = _replace s ""

remove_many :: [Text] -> String -> String
remove_many [] s = s
remove_many (x:xs) s = remove_many xs (remove x s)


mhead :: [[Char]] -> Stack
mhead (x:xs) | x /= "" = (head x):(mhead xs)
mhead ("":"":"":"":xs) = '-':(mhead xs)
mhead [] = []
mhead _  = error "empty"


lkup :: Ord k => k -> Map.Map k v -> v
lkup k = fromJust . Map.lookup k

apply_move :: Move -> StackMap -> StackMap
apply_move (n, frm_i, to_i) stack = 
    let (from, to) = (lkup frm_i stack, lkup to_i stack)
        (newfrom, newto) = move_n n (from, to) in
        Map.insert to_i newto . Map.insert frm_i newfrom $ stack
    
apply_moves :: [Move] -> StackMap -> StackMap
apply_moves [] s = s
apply_moves (x:xs) s = apply_moves xs $ apply_move x s


move_n :: Int -> (Stack, Stack) -> (Stack, Stack)
move_n n (from, to) = let (moving, rest) = splitAt n from in
    (rest, moving ++ to)
