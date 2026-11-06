
import Data.List.Split (splitOn)

main :: IO ()
main = do
    -- read contents of input1.txt:
    contents <- readFile "quest01/input1.txt"
    -- split on commas and convert to ints:
    let nums = map read (splitOn "," contents) :: [Int]
    -- find next largest square for each number:
    let nextSquares = map (\x -> let r = ceiling (sqrt (fromIntegral x)) in r * r) nums
    print $ sum $ zipWith (-) nextSquares nums
