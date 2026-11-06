import Data.List (sortBy)
import Data.Char (digitToInt)

main :: IO ()
main = do
    content <- fmap lines (readFile "quest02/input2.txt")
    print $ sum $ map checksum content
    where
        checksum line = read $ sortByHardness reach
            where
                edges = [(a,b) | [a, op, b] <- words line, (a,b) <- if op == '>' then [(a,b)] else [(b,a)]]
                reach = closure $ foldl addEdge initial edges
                initial = [[i == j | j <- [1..9]] | i <- [1..9]]
                addEdge r (a,b) = [[r !! (i-1) !! (j-1) || (i == digitToInt a && j == digitToInt b) | j <- [1..9]] | i <- [1..9]]
                closure r =
                    foldl (\m k -> [[m !! (i-1) !! (j-1) || (m !! (i-1) !! (k-1) && m !! (k-1) !! (j-1)) | j <- [1..9]] | i <- [1..9]]) r [1..9]
                sortByHardness r = sortBy (\a b -> if r !! (digitToInt a-1) !! (digitToInt b-1) then LT else GT) "123456789"
