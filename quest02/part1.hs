import Data.List (sortBy)

main :: IO ()
main = do
    lines <- fmap lines (readFile "quest02/input1.txt")
    print $ sum $ map checksum lines
    where
        checksum line = read $ sortBy (cmp line) "12345"
        cmp line a b
          | (a,b) `elem` wins = LT
          | otherwise = GT
          where
            wins = [ (x, y) | [x, op, y] <- words line, op == '>'] ++ [ (y, x) | [x, op, y] <- words line, op == '<']
