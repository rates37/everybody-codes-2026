
import Data.List.Split (splitOn)

main :: IO ()
main = do
    -- read contents of input2.txt:
    contents <- readFile "quest01/input2.txt"
    -- split on commas and convert to ints:
    let nums = map read (splitOn "," contents) :: [Int]

    -- find next largest value in the sequence (4n-4, n>3) for each number:
    let k x = max 3 (ceiling (fromIntegral (x + 4) / 4))

    let nextVals = map (\x -> let n = k x in 4 * n - 4) nums
    print $ sum $ zipWith (-) nextVals nums
