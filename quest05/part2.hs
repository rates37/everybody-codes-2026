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

initialSand :: [String] -> [String]
initialSand grid = [[let x = grid !! r !! c in if x == '#' && (r == 0 || r == length grid - 1 || c == 0 || c == length (head grid) - 1 || neighbours "01" grid r c >= 1) then '=' else x | c <- [0 .. length (head grid) - 1]] | r <- [0 .. length grid - 1]]

sandStep :: [String] -> [String]
sandStep grid =
  [[let x = grid !! r !! c in if x == '#' && neighbours "=" grid r c >= 5 then '=' else x | c <- [0 .. length (head grid) - 1]] | r <- [0 .. length grid - 1]]

solveSand :: [String] -> [String]
solveSand grid = let next = sandStep grid in if next == grid then grid else solveSand next

solve :: [String] -> [String]
solve grid = let next = step grid in if next == grid then grid else solve next

main :: IO ()
main = do
  grid <- fmap lines (readFile "quest05/input2.txt")
  let rows = length grid
      cols = length (head grid)
      center = (rows `div` 2, cols `div` 2)

      initial = [[if (r, c) == center then '#' else grid !! r !! c | c <- [0 .. cols - 1]] | r <- [0 .. rows - 1]]

      result = solve initial
      coastline = initialSand result
      beach = solveSand coastline
  print $ sum $ map (length . filter (== '=')) beach
