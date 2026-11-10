import Data.Array (Array, listArray, (!))
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

main :: IO ()
main = do
  content <- readFile "quest11/input2.txt"
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

      newStrain a b = 2 * sum [fromIntegral (minimum [d u v, d u a + 1 + d b v, d u b + 1 + d a v]) | u <- [1 .. n], v <- [u + 1 .. n]]
      best = minimum [newStrain a b | a <- [1 .. n], b <- [a + 1 .. n]]
      resonance = original - best
  --   mapM_ (\i -> print (i, bfs graph n i)) [1..n]
  --   print original
  --   print best
  print resonance
