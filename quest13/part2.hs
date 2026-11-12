{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (nub, sort)
import Data.Ratio ((%))

split :: String -> [String]
split [] = [""]
split (':' : xs) = "" : split xs
split (x : xs) = let (y : ys) = split xs in (x : y) : ys

lastWord :: String -> String
lastWord = last . words

parseInts :: String -> [Int]
parseInts = map read . split . lastWord

parseDirs :: String -> [Int]
parseDirs = map (\x -> if x == "CW" then 1 else -1) . split . lastWord

-- Position in revs at time t
position :: Int -> Int -> Int -> Int -> Int -> Rational
position n dir cur moves t = fromIntegral cur % fromIntegral n + fromIntegral (dir * min moves t) % fromIntegral n

-- Number of ints crossed in the interval (a,b]
crossed :: Rational -> Rational -> Integer
crossed a b
  | b > a = floor b - floor a
  | b < a = ceiling a - ceiling b
  | otherwise = 0

-- Count meetings between two hands
meetings :: [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> Integer
meetings ns ds cs ms i j = sum [crossed (diff l) (diff r) | (l, r) <- zip times (tail times)]
  where
    times = sort . nub $ [0, ms !! i, ms !! j]
    diff t = position (ns !! i) (ds !! i) (cs !! i) (ms !! i) t - position (ns !! j) (ds !! j) (cs !! j) (ms !! j) t

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
  input <- readFile "quest13/input2.txt"
  let clocks = parseClocks input
      cals = map solveClock clocks
      total = sum cals
  print total
