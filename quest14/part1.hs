main :: IO ()
main = do
  contents <- readFile "quest14/input1.txt"
  let rows = lines contents
      h = length rows
      w = length $ head rows
      at r c = rows !! r !! c
      count = sum [(if r == 0 || at r c /= at (r - 1) c then 1 else 0) + (if c == 0 || at r c /= at r (c - 1) then 1 else 0) | r <- [0 .. h - 1], c <- [0 .. w - 1]]
      total = count + h + w
  print total
