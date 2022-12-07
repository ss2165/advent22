module Day2 (solve) where

solve :: IO ()
solve = do
    contents <- readFile "inputs/day2.txt"
    let scores = (map score_round.rounds) contents

    print (sum scores)

data Hand = Rock | Paper | Scissors deriving (Enum, Show, Eq)
data Outcome = Win | Draw | Lose deriving (Enum, Show)
type Round = (Hand, Hand)


rounds:: [Char] -> [Round]
rounds contents = (map read_hand . lines) contents


read_first:: Char -> Hand
read_first x = case x of
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissors
    _ -> error "invalid letter"

-- read_second:: Char -> Hand
-- read_second x = case x of
--     'X' -> Rock
--     'Y' -> Paper
--     'Z' -> Scissors
--     _ -> error "invalid letter"

read_second:: Hand -> Char -> Hand
read_second them x = case x of
    'X' -> loses them
    'Y' -> them
    'Z' -> beats them
    _ -> error "invalid letter"

read_hand:: [Char] -> Round
read_hand lin  = let wds = words lin in
    case wds of 
        ([a]:[b]:[]) -> let them = read_first a in 
            (them, read_second them b)
        _ -> error "invalid round"

score_me:: Hand -> Int
score_me x = fromEnum x + 1

loses:: Hand -> Hand
loses x = case x of
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper

beats:: Hand -> Hand
beats x = case x of
    Scissors -> Rock
    Rock -> Paper
    Paper -> Scissors

outcome:: Round -> Outcome
outcome x = case x of 
    (a, y) | a == y -> Draw
    (a, b) | b == beats a -> Win
    _ -> Lose

score_outcome:: Outcome -> Int
score_outcome x = case x of 
    Win -> 6
    Draw -> 3
    Lose -> 0

score_round:: Round -> Int
score_round rnd@(_, me) = (score_outcome.outcome) rnd + score_me me