
solve :: [String] -> Integer
solve [iso, n] =
    let bases  = drop 4 iso
        digits = map (read . (:[])) (reverse n) :: [Integer] -- read . (:[]) converts each character to a string and then reads it as an Integer
        bases' = map (read . (:[])) (reverse bases) :: [Integer]
    in sum $ zipWith3 (\d b i -> d * b ^ i) digits bases' [0..]

solve _ = 0 -- unused


main :: IO ()
main = do
    content <- fmap lines (readFile "quest03/input1.txt")
    -- split on whitespace:
    let wordsContent = map words content
    print $ sum $ map solve wordsContent
