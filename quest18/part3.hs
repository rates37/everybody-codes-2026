import Data.List (sort)

parseLine :: String -> (Int, Int, Int)
parseLine line =
  let parts = words line
      x = read (drop 2 (parts !! 0))
      y = read (drop 2 (parts !! 1))
      w = read (drop 6 (parts !! 2))
   in (x, y, w)

solve :: [(Int, Int, Int)] -> Int
solve measurements =
  let heights = sort [y | (_, y, _) <- measurements]
      groups = zipWith (-) heights (0 : heights)
   in sum [rowWidth measurements h * count | (h, count) <- zip heights groups, count > 0]

rowWidth :: [(Int, Int, Int)] -> Int -> Int
rowWidth measurements row =
  let active =
        [(x, x + w - 1) | (x, height, w) <- measurements, height >= row]
   in case active of
        [] -> 0
        _ ->
          maximum (map snd active) - minimum (map fst active) + 1

main :: IO ()
main = do
  contents <- readFile "quest18/input3.txt"
  print $ solve $ map parseLine $ lines contents
