import Data.Array (Array, bounds, listArray, (!), (//))
import qualified Data.IntMap.Strict as M
import Data.List (foldl', nub)
import qualified Data.Sequence as Q

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs (x : y : xs) = (x, y) : pairs xs
pairs _ = []

bfs :: M.IntMap [Int] -> Int -> Int -> [Int]
bfs graph n s = go (Q.singleton s) (M.singleton s 0) 0
  where
    go queue dist _ =
      case Q.viewl queue of
        Q.EmptyL -> [dist M.! i | i <- [1 .. n]]
        v Q.:< rest ->
          let d = dist M.! v
              (q', dist', _) =
                foldl'
                  (visit (d + 1))
                  (rest, dist, 0)
                  (graph M.! v)
           in go q' dist' 0

    visit d (q, dist, total) u
      | M.member u dist = (q, dist, total)
      | otherwise = (q Q.|> u, M.insert u d dist, total + d)

matchings :: [Int] -> [[(Int, Int)]]
matchings [] = [[]]
matchings (x : xs) = [(x, y) : rest | y <- xs, rest <- matchings (filter (/= y) xs)]

floyd :: Int -> Array (Int, Int) Int -> Array (Int, Int) Int
floyd k a = a // [((i, j), min (a ! (i, j)) (a ! (i, k) + a ! (k, j))) | i <- [0 .. 7], j <- [0 .. 7]]

strain :: Int -> (Int -> Int -> Int) -> Array Int Int -> [(Int, Int)] -> Int
strain n d crystals matching =
  let initial = listArray ((0, 0), (7, 7)) [if i == j then 0 else d (crystals ! i) (crystals ! j) | i <- [0 .. 7], j <- [0 .. 7]]

      connected = initial // concat [[((a, b), 1), ((b, a), 1)] | (a, b) <- matching]
      shortest = foldl' (flip floyd) connected [0 .. 7]
   in 2 * sum [minimum (d u v : [d u (crystals ! i) + shortest ! (i, j) + d (crystals ! j) v | i <- [0 .. 7], j <- [0 .. 7]]) | u <- [1 .. n], v <- [u + 1 .. n]]

main :: IO ()
main = do
  content <- readFile "quest11/input3.txt"
  let (header : restLines) = lines content
      n = read (last (words header)) :: Int
      rest = map read (concatMap words restLines) :: [Int]
      edges = pairs rest

      base = M.fromList [(i, nub [wrap (i + k) | k <- [-3 .. 3], k /= 0, wrap (i + k) /= i]) | i <- [1 .. n]]
      wrap x = ((x - 1) `mod` n) + 1
      addEdge g (u, v) = M.adjust (v :) u $ M.adjust (u :) v g
      graph = foldl' addEdge base edges

      dist :: Array Int Int
      dist = listArray (0, n * n - 1) (concat [bfs graph n i | i <- [1 .. n]])
      d u v = dist ! ((u - 1) * n + v - 1)

      distBase = listArray (0, n * n - 1) (concat [bfs base n i | i <- [1 .. n]])

      original = sum [distBase ! ((u - 1) * n + v - 1) | u <- [1 .. n], v <- [1 .. n]]

      step = n `div` 8
      placements = [listArray (0, 7) [wrap (offset + i * step) | i <- [0 .. 7]] | offset <- [1 .. step]]
      best = minimum [strain n d crystals matching | crystals <- placements, matching <- matchings [0 .. 7]]
      resonance = original - best

  print resonance
