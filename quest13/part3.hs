{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (nub, sort)

split :: String -> [String]
split [] = [""]
split (':' : xs) = "" : split xs
split (x : xs) = let (y : ys) = split xs in (x : y) : ys

lastWord :: String -> String
lastWord = last . words

parseInts :: String -> [Integer]
parseInts = map read . split . lastWord

parseDirs :: String -> [Integer]
parseDirs = map (\x -> if x == "CW" then 1 else -1) . split . lastWord

ceilDiv :: Integer -> Integer -> Integer
ceilDiv a b = negate (negate a `div` b)

crossed :: Integer -> Integer -> Integer -> Integer
crossed a b d
  | b > a = b `div` d - a `div` d
  | b < a = ceilDiv a d - ceilDiv b d
  | otherwise = 0

-- Count meetings between two hands
meetings :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> Int -> Int -> Integer
meetings ns ds cs ms i j =
  sum [crossed (diff l) (diff r) d | (l, r) <- zip times (tail times)]
  where
    n1 = ns !! i
    n2 = ns !! j
    d = lcm n1 n2
    s1 = d `div` n1
    s2 = d `div` n2
    times = sort . nub $ [0, ms !! i, ms !! j]
    diff t = (cs !! i + ds !! i * min (ms !! i) t) * s1 - (cs !! j + ds !! j * min (ms !! j) t) * s2

parseClocks :: String -> [[String]]
parseClocks = filter (not . null) . splitOnBlank . lines
  where
    splitOnBlank [] = []
    splitOnBlank xs =
      let (clock, rest) = break null xs
       in clock : case rest of
            [] -> []
            (_ : remaining) -> splitOnBlank remaining

solveClock :: [String] -> Integer
solveClock ls =
  let ns = parseInts (ls !! 0)
      ds = parseDirs (ls !! 1)
      cs = parseInts (ls !! 2)
      ms = parseInts (ls !! 3)
      pairs = [(0, 1), (0, 2), (1, 2)]
   in sum [meetings ns ds cs ms i j | (i, j) <- pairs]

main :: IO ()
main = do
  input <- readFile "quest13/input3.txt"
  let clocks = parseClocks input
      cals = map solveClock clocks
      total = sum cals
  print total
