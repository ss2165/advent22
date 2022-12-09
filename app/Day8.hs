module Day8 (solve) where
import Data.List (transpose)

solve :: IO ()
solve = do
    contents <- readFile "inputs/day8.txt"

    let nums = readTrees contents
    let int = (repeat $ repeat False, nums)
    let final = (tup reflect . tup trap . tup reflect . tup id) int
    print ( numVis $ fst final)

    let init2 = (initVisDist nums, nums)
    let final2 = (tup2 reflect . tup2 trap . tup2 reflect . tup2 id) init2
    print ((maximum . map maximum ) $ fst final2)

initVisDist ::[[a]] -> [[Int]]
initVisDist nums = let (n_x, n_y) = (length nums, length $ head nums) in
    [mul_lst n_y 0] ++ mul_lst (n_x - 2)  ([0] ++ mul_lst (n_y - 2) 1 ++ [0]) ++ [mul_lst n_y 0]


mul_lst :: Int -> a -> [a]
mul_lst n x = take n (repeat x)

type State = ([[Bool]], [[Int]]) 
tup :: (State -> State) -> State -> State
tup f s = let (rvis, rnums) = f s in
    (subsq rnums rvis, rnums)

type State2 = ([[Int]], [[Int]]) 
tup2 :: (State2 -> State2) -> State2 -> State2
tup2 f s = let (rvis, rnums) = f s in
    (scanline visDist rnums rvis, rnums)

reflect :: ([[a]], [[b]]) -> ([[a]], [[b]])
reflect (x, y) = (map reverse x, map reverse y)

trap :: ([[a]], [[b]]) -> ([[a]], [[b]])
trap (x, y) = (transpose x, transpose y)

numVis :: [[Bool]] -> Int
numVis = sum . map (length . filter id)

readTrees :: String  -> [[Int]]
readTrees x = [[read (i:[]) | i <- lin ] | lin <- (lines x)]

subsq :: [[Int]] -> [[Bool]] -> [[Bool]]
subsq  = scanline visible

scanline :: ([b] -> [a] -> [b]) -> [[a]] -> [[b]] -> [[b]]
scanline scanner nums bols = [scanner bol num | (num, bol) <- zip nums bols]
-- subsq nums bols = map (\x -> visible (snd x) (fst x)) (zip nums bols)

visible:: [Bool] -> [Int] -> [Bool]
visible l b = _vis b l (-1) where
    _vis (_:_) [] _ = error "must be the same length"
    _vis [] _ _ = []
    _vis (x:xs) (y:ys) biggest = (isvis x biggest y):(_vis xs ys (max x biggest))


isvis :: Int -> Int -> Bool -> Bool
isvis cur biggest vis = vis || (cur > biggest)

visDist :: [Int] -> [Int] -> [Int]
visDist [] _ = []
visDist _ [] = []
visDist (x:xs) (y:ys) | x == 0 = 0:rest
                      | otherwise = (len*x):rest
                      where rest = (visDist xs ys)
                            (smaller, remain) = span (<y) ys
                            smal_len = length smaller
                            len = if remain == [] then smal_len else smal_len + 1
                        
