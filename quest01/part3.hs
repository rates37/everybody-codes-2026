
import Data.List.Split (splitOn)

main :: IO ()
main = do
    -- read contents of input3.txt:
    contents <- readFile "quest01/input3.txt"
    -- split on commas and convert to ints:
    let nums = map read (splitOn "," contents) :: [Int]
    -- find next largest odd square for each number:
    let s x = let r = ceiling (sqrt (fromIntegral (x + 1))) in if odd r && r >= 3 then r else r + 1
    let nextSquares = map (\x -> let r = s x in r * r - 1) nums
    print $ sum $ zipWith (-) nextSquares nums
