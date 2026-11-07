neighbours :: String -> [String] -> Int -> Int -> Int
neighbours set grid r c =
  length
    [ () | dr <- [-1 .. 1], dc <- [-1 .. 1], (dr, dc) /= (0, 0), let nr = r + dr, let nc = c + dc, nr >= 0, nr < length grid, nc >= 0, nc < length (head grid), grid !! nr !! nc `elem` set
    ]

step :: [String] -> [String]
step grid =
  [ [ let x = grid !! r !! c
       in if x /= '#' && neighbours "#" grid r c >= 1 && neighbours "#1" grid r c >= 5
            then '#'
            else x
      | c <- [0 .. length (head grid) - 1]
    ]
    | r <- [0 .. length grid - 1]
  ]

solve :: [String] -> [String]
solve grid = let next = step grid in if next == grid then grid else solve next

main :: IO ()
main = do
  grid <- fmap lines (readFile "quest05/input1.txt")
  let rows = length grid
      cols = length (head grid)
      center = (rows `div` 2, cols `div` 2)

      initial = [[if (r, c) == center then '#' else grid !! r !! c | c <- [0 .. cols - 1]] | r <- [0 .. rows - 1]]
      result = solve initial
  print $ sum $ map (length . filter (== '#')) result
