import qualified Data.IntSet as IS
import qualified Data.Map as M

dirs :: [(Int, Int)]
dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]

-- we only search up to this much sulphur, the rest is extrapolated
limit :: Int
limit = 1500

bestTimes :: [String] -> M.Map Int Int
bestTimes rows = bfs 0 [(0, sr, sc, 4)] (IS.singleton (key (0, sr, sc, 4))) M.empty
  where
    h = length rows
    w = maximum (map length rows)
    at r c = if c < length (rows !! r) then rows !! r !! c else '#'
    find ch = head [(r, c) | r <- [0 .. h - 1], c <- [0 .. w - 1], at r c == ch]
    (sr, sc) = find 'S'
    goal = find 'E'

    key (g, r, c, d) = ((g * h + r) * w + c) * 5 + d

    next (g, r, c, d) =
      [ (g', nr, nc, nd)
        | nd <- [0 .. 3],
          d == 4 || nd /= (d + 2) `mod` 4,
          let (dr, dc) = dirs !! nd,
          let nr = r + dr,
          let nc = c + dc,
          nr >= 0,
          nr < h,
          nc >= 0,
          nc < w,
          at nr nc /= '#',
          let g' = if at nr nc == '@' then g + 1 else g,
          g' <= limit
      ]

    bfs _ [] _ found = found
    bfs t frontier seen found = bfs (t + 1) fresh seen' found'
      where
        found' = foldl (\m (g, r, c, _) -> if (r, c) == goal then M.insertWith (\_ old -> old) g t m else m) found frontier
        (fresh, seen') = foldl visit ([], seen) (concatMap next frontier)
        visit (acc, s) st
          | IS.member (key st) s = (acc, s)
          | otherwise = (st : acc, IS.insert (key st) s)

solve :: [String] -> Int
solve rows = base + (target - g0) `div` period * extra
  where
    target = 202620262026
    best = bestTimes rows
    half = limit `div` 2
    -- the shortest period where every extra `period` sulphur costs the same extra time
    (period, extra) = head [(a, e) | a <- [1 .. half `div` 2], let e = best M.! limit - best M.! (limit - a), all (\g -> best M.! (g + a) - best M.! g == e) [half .. limit - a]]
    g0 = head [g | g <- [half ..], g `mod` period == target `mod` period]
    base = best M.! g0

main :: IO ()
main = do
  input <- readFile "quest15/input3.txt"
  print $ solve $ lines input
