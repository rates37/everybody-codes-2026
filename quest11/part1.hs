import qualified Data.IntMap.Strict as M
import Data.List (foldl')
import qualified Data.Sequence as Q

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs (x : y : xs) = (x, y) : pairs xs
pairs _ = []

bfs :: M.IntMap [Int] -> Int -> Int
bfs graph s = go (Q.singleton s) (M.singleton s 0) 0
  where
    go queue dist total =
      case Q.viewl queue of
        Q.EmptyL -> total
        v Q.:< rest ->
          let d = dist M.! v
              (q', dist', total') = foldl' (visit (d + 1)) (rest, dist, total) (graph M.! v)
           in go q' dist' total'
    visit d (q, dist, total) u
      | M.member u dist = (q, dist, total)
      | otherwise = (q Q.|> u, M.insert u d dist, total + d)

main = do
  content <- readFile "quest11/input1.txt"
  let (header : restLines) = lines content
      n = read (last (words header)) :: Int
      rest = map read (concatMap words restLines) :: [Int]
      edges = pairs rest
      graph = foldl' addEdge base edges

      base = M.fromList [(i, [prev i, next i]) | i <- [1 .. n]]
      prev i = if i == 1 then n else i - 1
      next i = if i == n then 1 else i + 1
      addEdge g (u, v) = M.adjust (v :) u $ M.adjust (u :) v g
      resonance = n * (n `div` 2) ^ 2 - sum [bfs graph i | i <- [1 .. n]]
  print resonance
