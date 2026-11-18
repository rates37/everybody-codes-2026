import qualified Data.Sequence as Q
import qualified Data.Set as S
import Data.Foldable (toList)


bfs :: [String] -> Int -> Int -> (Int, Int) -> (Int, Int) -> Int
bfs grid h w start target = go (Q.singleton (start, 0)) (S.singleton start)
    where
        go queue visited = 
            case Q.viewl queue of
                Q.EmptyL -> -1
                ((pos@(r,c), dist) Q.:< rest)
                    | pos == target -> dist
                    | otherwise ->
                        let neighbours = filter valid [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
                            valid (x,y) = x >= 0 && x < h && y >= 0 && y < w && (grid !! x !! y == '#') && S.notMember (x,y) visited
                            newVisited = foldr S.insert visited neighbours
                            newQueue = rest Q.>< Q.fromList [(p, dist+1) | p <- neighbours]
                        in go newQueue newVisited

main :: IO ()
main = do
    content <- readFile "quest17/input1.txt"
    let grid = lines content
        h = length grid
        w = length (head grid)
        start = (0,0)
        target = (h-1, w-1)
    print $ bfs grid h w start target
