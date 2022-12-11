module Day10 (solve) where
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

window_size :: Int
window_size = 14

solve :: IO ()
solve = do
    -- contents <- readFile "inputs/day10_trial.txt"
    contents <- readFile "inputs/day10.txt"
    let all_updates = callCycles 1 1 $ readComs contents
    let all_cycles = allXVals (1, 1) all_updates
    let ns = [20, 60, 100, 140, 180, 220]

    print (signalStrn ns all_updates)
    putStr (displ all_cycles)

type Com = Maybe Int
type Cycle = Int
type XVal = Int
readComs :: String -> [Com]
readComs s = map _com $ lines s where
    _com l = case splitOn " " l of
        ["addx", i] -> Just (read i)
        ["noop"] -> Nothing
        x -> error (concat x) 

callCycles :: XVal -> Cycle -> [Com] -> [(Cycle, XVal)]
callCycles _ _ [] = []
callCycles x c (com:coms) = case com of
    Nothing -> callCycles x (c+1) coms
    Just i -> let next_cyc = c + 2 
                  x' = x + i in
        (next_cyc, x'):(callCycles x' next_cyc coms)


findLast :: (a -> Bool) -> [a] -> Maybe (a, [a])
findLast _ [] = Nothing
findLast f [x] | f x = Just (x, [])
               | otherwise = Nothing
findLast f (x:xs) | f x && (not.f.head) xs = Just (x, xs)
                  | otherwise = findLast f xs

nthXVals :: [Int] -> [(Cycle, XVal)] -> [Int]
nthXVals [] _ = []
nthXVals _ [] = []
nthXVals (n:ns) updates  = let ((_, xv), rest) = fromJust $ findLast (\x -> fst x <= n) updates in
    xv:(nthXVals ns rest)

signalStrn :: [Int] -> [(Cycle, XVal)] -> Int
signalStrn ns updates = let nvals = nthXVals ns updates in
    sum [i*j | (i, j) <- zip ns nvals]


pixel :: Cycle -> XVal -> String
pixel c x | (px >= (x-1)) && (px <= (x + 1)) = '#':r
          | otherwise = '.':r
          where px = c `mod` 40
                r = if px == 39 then "\n" else ""


allXVals :: (Cycle, XVal) -> [(Cycle, XVal)] -> [XVal]
allXVals _ [] = []
allXVals (cur_c, start) ((c, x):rest) = (take (c - cur_c) $ repeat start) ++ (allXVals (c, x) rest)


displ :: [XVal] -> String
displ xs = concat [pixel i x | (i, x) <- zip [0..] xs ]