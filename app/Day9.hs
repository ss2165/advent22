module Day9 (solve, updateT) where
import Data.List.Split (splitOn)
import Data.Set (fromList)

type Point = (Int, Int)
type Com = (Char, Int)

solve :: IO ()
solve = do
    contents <- readFile "inputs/day9.txt"
    let coms = readComs contents
    let hs = applyAllComs coms (0, 0)

    print (length $ fromList $ simNKnots 1 hs)
    print (length $ fromList $ simNKnots 9 hs)


readComs :: String -> [Com]
readComs x = let spl s = (head $ head s, read . head $ tail s) 
                 spot = spl . splitOn " " in
                 map spot $ lines x
                
simNKnots :: Int -> [Point] -> [Point]
simNKnots 0 a = a
simNKnots n a = simNKnots (n-1) (trackT (0, 0) a)


dirFunc :: Char -> Point -> Point
dirFunc dir (x, y) = case dir of 
        'R' -> (x + 1, y)
        'L' -> (x - 1, y)
        'U' -> (x, y + 1)
        'D' -> (x, y - 1)
        e -> error ("invalid command" ++ [e])

applyCom :: Com -> Point -> [Point]
applyCom (dir, n) p = _comLst p n where
        dirF = dirFunc dir
        _comLst k m | m == 0 = []
                    | otherwise = let newpoint = dirF k in 
                        newpoint:(_comLst newpoint (m-1)) 


applyAllComs :: [Com] -> Point -> [Point]
applyAllComs [] _ = []
applyAllComs (p:ps)  x = let nexts = applyCom p x in
    nexts ++ (applyAllComs ps (last nexts))

updateT :: Point -> Point -> Point
updateT (hx, hy) t@(tx, ty) | (abs dx) < 2 && (abs dy) < 2 = t
                            | otherwise = (tx + (signum dx), ty + (signum dy))
                            where dx = hx - tx
                                  dy = hy - ty


trackT :: Point -> [Point] -> [Point]
trackT _ [] = []
trackT t (h:hs) = let nt = updateT h t in nt:(trackT nt hs)