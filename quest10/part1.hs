import qualified Data.Map as M

type Ghost = (Int, Int, Char)

clockwise :: Char -> Char
clockwise '>' = 'v'
clockwise 'v' = '<'
clockwise '<' = '^'
clockwise '^' = '>'
clockwise c = c

reverseDir :: Char -> Char
reverseDir '>' = '<'
reverseDir 'v' = '^'
reverseDir '<' = '>'
reverseDir '^' = 'v'
reverseDir c = c

move :: [String] -> Ghost -> (Ghost, Int)
move grid (r, c, d) =
  let (nr, nc) = case d of
        '>' -> (r, c + 1)
        'v' -> (r + 1, c)
        '<' -> (r, c - 1)
        '^' -> (r - 1, c)
        _ -> (r, c)
   in if grid !! nr !! nc == '#' then ((r, c, clockwise d), 1) else ((nr, nc, d), 0)

roundSim :: [String] -> [Ghost] -> ([Ghost], Int)
roundSim grid ghosts =
  let moved = map (move grid) ghosts
      positions = map fst moved
      wallHits = sum (map snd moved)
      groups = M.fromListWith (+) [((r, c), 1 :: Int) | (r, c, _) <- positions]
      colisions = sum [n | n <- M.elems groups, n > 1]
      final = map (\(r, c, d) -> if M.findWithDefault 0 (r, c) groups > 1 then (r, c, reverseDir d) else (r, c, d)) positions
   in (final, wallHits + colisions)

simulate :: [String] -> [Ghost] -> Int
simulate grid = go 10 0
  where
    go 0 acc _ = acc
    go n acc ghosts =
      let (newGhosts, hits) = roundSim grid ghosts
       in go (n - 1) (acc + hits) newGhosts

main :: IO ()
main = do
  grid <- lines <$> readFile "quest10/input1.txt"

  print $ simulate grid [(r, c, d) | (r, row) <- zip [0 ..] grid, (c, d) <- zip [0 ..] row, d `elem` "<>^v"]
