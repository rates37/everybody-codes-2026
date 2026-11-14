import Data.Array (listArray, (!))
import qualified Data.IntSet as IS
import Data.Sequence (ViewL (..), (|>))
import qualified Data.Sequence as Q

dirs :: [(Int, Int)]
dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]

solve :: [String] -> Int
solve rows = bfs initial IS.empty (Q.singleton (initial, 0))
  where
    h = length rows
    w = maximum (map length rows)
    grid = listArray (0, h * w - 1) [if c < length (rows !! r) then rows !! r !! c else '#' | r <- [0 .. h - 1], c <- [0 .. w - 1]]

    findCell ch = head [i | i <- [0 .. h * w - 1], grid ! i == ch]
    start = findCell 'S'
    goal = findCell 'E'
    target = 2026
    initial = (start, 4, 0)
    encode (p, d, s) = (p * 5 + d) * (target + 1) + s

    bfs initial visited queue =
      case Q.viewl queue of
        EmptyL -> -1
        ((p, d, s), dist) :< rest
          | p == goal && s == target -> dist
          | IS.member key visited -> bfs initial visited rest
          | otherwise -> bfs initial visited' (foldl (|>) rest next)
          where
            key = encode (p, d, s)
            visited' = IS.insert key visited
            r = p `div` w
            c = p `mod` w
            next =
              [ ((np, nd, ns), dist + 1)
                | nd <- [0 .. 3],
                  d == 4 || nd /= (d + 2) `mod` 4,
                  let (dr, dc) = dirs !! nd,
                  let nr = r + dr,
                  let nc = c + dc,
                  nr >= 0,
                  nr < h,
                  nc >= 0,
                  nc < w,
                  let np = nr * w + nc,
                  grid ! np /= '#',
                  let ns =
                        min
                          target
                          (s + if grid ! np == '@' then 1 else 0),
                  not (IS.member (encode (np, nd, ns)) visited')
              ]

main :: IO ()
main = do
  input <- readFile "quest15/input2.txt"
  print $ solve $ lines input
