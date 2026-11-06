
solve :: [String] -> Integer
solve [iso, n] =
    let bases  = drop 4 iso
        digits = map (read . (:[])) (reverse n) :: [Integer]
        bases' = map (read . (:[])) (take (length digits) (cycle (reverse bases))) :: [Integer]
    in sum $ zipWith3 (\d b i -> d * b ^ i) digits bases' [0..]

solve _ = 0 -- unused


main :: IO ()
main = do
    content <- fmap lines (readFile "quest03/input2.txt")
    -- split on whitespace:
    let wordsContent = map words content
    print $ sum $ map solve wordsContent
