import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Pos = (Int, Int)

makeRuns :: S.Set Pos -> Bool -> (M.Map Pos Int, Int)
makeRuns magma horizontal = foldl' assign (M.empty, 0) (S.toList magma)
  where
    adjacent (r, c)
      | horizontal = [(r, c - 1), (r, c + 1)]
      | otherwise = [(r - 1, c), (r + 1, c)]

    assign (mp, n) p
      | M.member p mp = (mp, n)
      | otherwise =
          let run = collect [p] S.empty
              mp' = foldl' (\m x -> M.insert x n m) mp run
           in (mp', n + 1)

    collect [] _ = []
    collect (p : queue) seen
      | S.member p seen = collect queue seen
      | S.notMember p magma = collect queue seen
      | otherwise = p : collect (adjacent p ++ queue) (S.insert p seen)

maxMatching :: [[Int]] -> Int
maxMatching graph = go 0 M.empty
  where
    go u matching
      | u == length graph = M.size matching
      | otherwise =
          let (_, matching') = augment u S.empty matching
           in go (u + 1) matching'

    augment u seen matching = try (graph !! u) seen matching
      where
        try [] _ m = (False, m)
        try (v : vs) visited m
          | S.member v visited = try vs visited m
          | otherwise =
              let visited' = S.insert v visited
               in case M.lookup v m of
                    Nothing -> (True, M.insert v u m)
                    Just old ->
                      let (found, m') = augment old visited' m
                       in if found then (True, M.insert v u m') else try vs visited' m'

solve :: [String] -> Int
solve rows =
  let magma =
        S.fromList [(r, c) | (r, row) <- zip [0 ..] rows, (c, ch) <- zip [0 ..] row, ch == '#']

      (hMap, hCount) = makeRuns magma True
      (vMap, _) = makeRuns magma False

      graph = [S.toList $ S.fromList [vMap M.! p | p <- S.toList magma, hMap M.! p == h] | h <- [0 .. hCount - 1]]
   in maxMatching graph

main :: IO ()
main = do
  contents <- readFile "quest12/input2.txt"
  print (solve (lines contents))
