import Data.Char (digitToInt, intToDigit, toUpper)
import Numeric (showIntAtBase)

segmentCount :: Char -> Int
segmentCount c = maybe (digitSegments !! digitToInt c) id (lookup c [('+', 2), ('-', 1), ('=', 2)])
  where
    digitSegments = [6, 2, 5, 5, 4, 5, 6, 3, 7, 6, 6]

isCorrect :: String -> Bool
isCorrect s = case words s of
  [a, o1, b, "=", c, o2, d] -> apply a o1 b == apply c o2 d
  _ -> False
  where
    apply x op y = fromDigits x `f` fromDigits y where f = if op == "+" then (+) else (-)
    fromDigits = foldl (\acc c -> acc * 11 + digitToInt c) 0


segments :: String -> Int
segments = sum . map segmentCount . filter (/= ' ')

main :: IO ()
main = do
  eqs <- lines <$> readFile "quest20/input2.txt"
  let total = sum [segments e | e <- eqs, isCorrect e]
  putStrLn (map toUpper (showIntAtBase 11 intToDigit total ""))
