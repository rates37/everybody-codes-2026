import Data.Char (digitToInt)

segmentCount :: Char -> Int
segmentCount c = maybe (digitSegments !! digitToInt c) id (lookup c [('+', 2), ('-', 1), ('=', 2)])
  where
    digitSegments = [6, 2, 5, 5, 4, 5, 6, 3, 7, 6]

isCorrect :: String -> Bool
isCorrect s = case words s of
  [[a], o1, [b], "=", [c], o2, [d]] -> apply a o1 b == apply c o2 d
  _ -> False
  where
    apply x op y = digitToInt x `f` digitToInt y where f = if op == "+" then (+) else (-)

segments :: String -> Int
segments = sum . map segmentCount . filter (/= ' ')

main :: IO ()
main = do
  eqs <- lines <$> readFile "quest20/input1.txt"
  print (sum [segments e | e <- eqs, isCorrect e])
