import Data.Array (accumArray, listArray, (!))
import Data.Bits (Bits (bit, (.|.)))
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
    deposits = [i | i <- [0 .. h * w - 1], grid ! i == '@']
    k = length deposits
    masks = 2 ^ k
    bits = accumArray (.|.) 0 (0, h * w - 1) [(p, bit i) | (i, p) <- zip [0 ..] deposits]
    initial = (start, 4, 0)
    encode (p, d, m) = (p * 5 + d) * masks + m

    bfs _ _ queue | Q.null queue = -1
    bfs s visited queue =
      case Q.viewl queue of
        EmptyL -> -1
        ((p, d, m), dist) :< rest
          | p == goal && m == masks - 1 -> dist
          | IS.member key visited -> bfs s visited rest
          | otherwise -> bfs s visited' (foldl (|>) rest next)
          where
            key = encode (p, d, m)
            visited' = IS.insert key visited
            r = p `div` w
            c = p `mod` w
            next =
              [ ((np, nd, m .|. bits ! np), dist + 1)
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
                  let nm = m .|. bits ! np,
                  not (IS.member (encode (np, nd, nm)) visited')
              ]

main :: IO ()
main = do
  input <- readFile "quest15/input1.txt"
  print $ solve $ lines input
