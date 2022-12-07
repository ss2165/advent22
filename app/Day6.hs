module Day6 (solve) where
import Data.Set (Set, insert, empty)

window_size :: Int
window_size = 14

solve :: IO ()
solve = do
    contents <- readFile "inputs/day6.txt"
    let buf_idx = fmap (+window_size) . findIndex all_diff . windows window_size
    print (map buf_idx $ lines contents)


windows :: Int -> [a] -> [[a]]
windows n lst = if (length lst) > n then (take n lst):(windows n $ tail lst) else [lst]

all_diff :: Ord a => [a] -> Bool
all_diff lst = length lst == length (fromList lst)

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex f lst = _fin f lst 0 where
    _fin g (x:xs) n = if f x then Just n else _fin g xs (n+1)
    _fin _ [] _ = Nothing


fromList :: Ord a => [a] -> Set a
fromList (x:xs) = insert x $ fromList xs
fromList [] = empty