import Data.Bits (complement, popCount, shiftL, (.&.), (.|.))
import Data.Char (digitToInt, intToDigit, toUpper)
import Data.Maybe (fromMaybe)
import Numeric (showIntAtBase)

mask :: Char -> Int
mask c = fromMaybe 0 (lookup c table)
  where
    table = [(d, sum [1 `shiftL` i | i <- bits]) | (d, bits) <- segs]
    segs =
      [ ('0', [0, 1, 2, 3, 4, 5]),
        ('1', [1, 2]),
        ('2', [0, 1, 3, 4, 6]),
        ('3', [0, 1, 2, 3, 6]),
        ('4', [1, 2, 5, 6]),
        ('5', [0, 2, 3, 5, 6]),
        ('6', [0, 2, 3, 4, 5, 6]),
        ('7', [0, 1, 2]),
        ('8', [0 .. 6]),
        ('9', [0, 1, 2, 3, 5, 6]),
        ('A', [0, 1, 2, 4, 5, 6]),
        ('+', [7, 8]),
        ('-', [7]),
        ('=', [7, 9])
      ]

-- Characters a symbol can change into
alternatives :: Char -> String
alternatives c
  | c `elem` ['0' .. '9'] ++ "A" = ['0' .. '9'] ++ "A"
  | c `elem` "+-" = "+-"
  | otherwise = [c]

-- Every way to move exactly one segment
oneMoves :: String -> [String]
oneMoves s = within ++ between
  where
    ix = [0 .. length s - 1]
    set i c str = take i str ++ [c] ++ drop (i + 1) str
    options i = [(n, popCount (o .&. complement m), popCount (m .&. complement o)) | let o = mask (s !! i), n <- alternatives (s !! i), let m = mask n]
    within = [set i n s | i <- ix, (n, 1, 1) <- options i]
    between = [set j n' (set i n s) | i <- ix, j <- ix, i /= j, (n, 1, 0) <- options i, (n', 0, 1) <- options j]

isCorrect :: String -> Bool
isCorrect s = case words s of
  [a, o1, b, "=", c, o2, d] -> apply a o1 b == apply c o2 d
  _ -> False
  where
    apply x op y = fromBase11 x `f` fromBase11 y where f = if op == "+" then (+) else (-)
    fromBase11 = foldl (\acc c -> acc * 11 + digitToInt c) 0

segments :: String -> Int
segments = sum . map (popCount . mask)

main :: IO ()
main = do
  content <- readFile "quest20/input3.txt"
  let eqs = filter (not . null) . lines $ content
      total = sum [segments e | e <- eqs, isCorrect e || any isCorrect (oneMoves e)]
  putStrLn (map toUpper (showIntAtBase 11 intToDigit total ""))
