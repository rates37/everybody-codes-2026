import qualified Data.Set as S

type Pos = (Int, Int)

neighbours :: Pos -> [Pos]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

bfs :: S.Set Pos -> Pos -> (S.Set Pos, [Pos])
bfs grid start = go (S.singleton start) [start]
  where
    go visited [] = (visited, [])
    go visited (p : queue) =
      let ns = filter (\x -> S.member x grid && S.notMember x visited) (neighbours p)
          visited' = foldr S.insert visited ns
          (finalVisited, rest) = go visited' (queue ++ ns)
       in (finalVisited, p : rest)

solve :: [String] -> Int
solve rows = go magma 0
  where
    magma = S.fromList [(r, c) | (r, row) <- zip [0 ..] rows, (c, ch) <- zip [0 ..] row, ch == '#']
    go remaining total
      | S.null remaining = total
      | otherwise =
          let start = S.findMin remaining
              (visited, component) = bfs remaining start
              rs = map fst component
              cs = map snd component
              h = maximum rs - minimum rs + 1
              w = maximum cs - minimum cs + 1
           in go (remaining `S.difference` visited) (total + min h w)

main :: IO ()
main = do
  contents <- readFile "quest12/input1.txt"
  print (solve (lines contents))
