distance :: [String] -> Int -> Int -> Int
distance grid h w = sum [1 | r <- [0 .. h - 1], c <- [0 .. w - 1], grid !! r !! c == '#', (dr, dc) <- [(-1, 0), (1, 0), (0, -1), (0, 1)], let nr = r + dr, let nc = c + dc, nr < 0 || nr >= h || nc < 0 || nc >= w || grid !! nr !! nc == '.']

main :: IO ()
main = do
  content <- readFile "quest17/input2.txt"
  let grid = lines content
      h = length grid
      w = length (head grid)
  print $ distance grid h w
