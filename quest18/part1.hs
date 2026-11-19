import Data.List (sort)

parseLine :: String -> (Int, Int)
parseLine line =
  let parts = words line
      x = read (drop 2 (parts !! 0))
      y = read (drop 2 (parts !! 1))
   in (x, y)

solve :: [(Int, Int)] -> Int
solve measurements =
  let stacks = sort measurements
      maxHeight = maximum (map snd stacks)
   in sum [rowWidth stacks y | y <- [1 .. maxHeight]]

rowWidth :: [(Int, Int)] -> Int -> Int
rowWidth stacks y =
  let cols = [x | (x, height) <- stacks, height >= y]
   in case cols of
        [] -> 0
        _ -> maximum cols - minimum cols + 1

main :: IO ()
main = do
  contents <- readFile "quest18/input1.txt"
  print $ solve $ map parseLine $ lines contents
