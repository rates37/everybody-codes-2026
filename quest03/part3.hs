
import Data.List (cycle)
import Data.Char (digitToInt)

solve :: [String] -> Integer
solve [iso, n] =
    let
        bases  = map (toInteger . digitToInt) (reverse (drop 5 iso))
        vote = read n :: Integer
        result = value bases vote
        valid = all ((/= result) . value bases) [0 .. vote-1]

        value :: [Integer] -> Integer -> Integer
        value bs nn = sum $ zipWith3 (\d b i -> toInteger (digitToInt d) * b ^ i) (reverse (show nn)) (cycle bs) [0..]

    in if valid then result else 0

solve _ = 0 -- unused


main :: IO ()
main = do
    content <- fmap lines (readFile "quest03/input3.txt")
    -- split on whitespace:
    let wordsContent = map words content
    print $ sum $ map solve wordsContent
