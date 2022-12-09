module Day7 (solve) where
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
solve :: IO ()
solve = do
    contents <- readFile "inputs/day7.txt"
    -- let tr = File 2
    -- let tr1 = treeFromList [("b", treeFromList [("fil2", tr)])]
    -- let tr2 = treeFromList [("a", tr1), ("fil3", File 3)]
    -- let tr3 = insertTree ["a", "b", "fil4"] (File 4) tr2
    -- let tr3 = Branch $ Map.fromList [("t", tr2), ("x", File 5)]
    -- let tr = addTree "a" (addFile ("me", 2) []) []
    -- let tr1 = addTree "c" (addFile ("you", 4) []) []
    -- let tr2 = addTree "d" tr1 (addTree "b" tr [])
    -- print tr2
    -- print (dir_sizes tr2)
    -- print (listSizes "/" $ dir_sizes tr3)
    let commands = map trim $ filter (/= "") $ splitOn "$" contents
    let comio = map ((\x ->(head x, tail x)).lines) commands

    let all_sizes = listSizes ("/", dir_sizes $ buildTree comio)


    print (sum $ filter (<=100000) $ snd $ unzip all_sizes)
    let cur_used = snd $ head all_sizes
    let need_to_delete = 30000000 - (70000000 - cur_used)
    let candidates = filter (\x -> snd x>=need_to_delete) all_sizes
    let comp x y = compare (snd x) (snd y)
    print (minimumBy comp candidates)

    
trim :: String -> String
trim x = case x of
    ' ':s -> s
    _ -> x


type FileData = Int
type SubTree = Map.Map String Tree
type Command = (String, [String])

data Tree = Branch SubTree | File FileData deriving (Show)

treeFromList :: [(String, Tree)] -> Tree
treeFromList = Branch . Map.fromList


insertTree :: [String] -> Tree -> Tree -> Tree
insertTree [] _ _ = error "needs path"
insertTree pth _ (File _) = error $ concat ("can't insert at file: ":pth)
insertTree pth subt (Branch mp) = case pth of
    [p] -> Branch $ Map.insert p subt mp
    (p:ps) -> Branch $ Map.adjust (insertTree ps subt) p mp


buildTree :: [Command] -> Tree
buildTree c = _buildTree c ((Branch Map.empty), []) where
    _buildTree (x:xs) state = _buildTree xs $ applyCommand x state
    _buildTree [] (t, _) = t

applyCommand :: Command -> (Tree, [String]) -> (Tree, [String])
applyCommand (com, outs) (t, path) = case com of 
    'c':'d':' ':dir -> (t, cd dir path)
    "ls" | path == [] -> (makeTree outs, path)
    "ls" ->    (insertTree (reverse path) (makeTree outs) t, path)
    x -> error $ "invalid command" ++ x

cd :: String -> [String] -> [String]
cd "/" _ = []
cd ".." (_:xs) = xs
cd x xs = x:xs


makeTree :: [String] -> Tree
makeTree [] = Branch Map.empty
makeTree (x:xs) = case split2 x of
    ("dir", dirnam) -> insertTree [dirnam] (Branch Map.empty) $ makeTree xs
    (size, fil) -> insertTree [fil] (File $ read size) $ makeTree xs


split2:: String -> (String, String)
split2 x = let spls = splitOn " " x in 
    (head spls, head $ tail spls)


data TreeSizes = T (Int, [(String,TreeSizes)]) deriving (Show)

dir_sizes :: Tree -> TreeSizes
dir_sizes (File _) = error "not valid on files"
dir_sizes (Branch mp) = T $ Map.foldrWithKey _fld (0, []) mp where
    _fld nam subt acc = case subt of
        File s -> (s + fst acc, (snd acc))
        Branch _ -> let (T subsizes) = dir_sizes subt in
            (fst subsizes + fst acc, (nam, T subsizes):(snd acc))


listSizes :: (String, TreeSizes) -> [(String, Int)]
listSizes (pth, (T (fsiz, subts))) = (pth, fsiz):(concat $ map listSizes subts)
