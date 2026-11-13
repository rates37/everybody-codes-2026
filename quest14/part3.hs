import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = do
  s <- readFile "quest14/input3.txt"
  let g = lines s
      h = length g
      w = length (head g)
      at (r, c) = g !! r !! c
      inb (r, c) = r >= 0 && r < h && c >= 0 && c < w
      nbrs (r, c) = filter inb [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

      flood seen [] = seen
      flood seen (p : ps)
        | S.member p seen = flood seen ps
        | otherwise = flood (S.insert p seen) ([q | q <- nbrs p, at q == at p] ++ ps)
      label (m, n) p
        | M.member p m = (m, n)
        | otherwise =
            let comp = flood S.empty [p]
             in (foldr (\q -> M.insert q n) m (S.toList comp), n + 1)
      cells = [(r, c) | r <- [0 .. h - 1], c <- [0 .. w - 1]]
      (lab, cnt) = foldl label (M.empty, 0 :: Int) cells

      shared = M.fromListWith (+) [((min a b, max a b), 1 :: Int) | p@(r, c) <- cells, q <- [(r + 1, c), (r, c + 1)], inb q, let a = lab M.! p, let b = lab M.! q, a /= b]
      border = M.fromListWith (+) [((-1, lab M.! p), 1 :: Int) | p@(r, c) <- cells, k <- [r == 0, r == h - 1, c == 0, c == w - 1], k]
      allEdges = M.toList border ++ M.toList shared

      -- Try placing charges on pieces x and y
      mst x y =
        let edges = sortOn snd [((a, b), if a == x || b == x || a == y || b == y then 0 else cost) | ((a, b), cost) <- allEdges]

            find m x =
              let p = M.findWithDefault x x m
               in if p == x then x else find m p

            step (m, t) ((a, b), cost)
              | ra == rb = (m, t)
              | otherwise = (M.insert ra rb m, t + cost)
              where
                ra = find m a
                rb = find m b
         in snd $ foldl step (M.empty, 0) edges

      pairs
        | cnt < 2 = [(0, 0)]
        | otherwise = [(a, b) | a <- [0 .. cnt - 1], b <- [a + 1 .. cnt - 1]]

  print $ minimum [mst a b | (a, b) <- pairs]
