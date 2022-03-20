import qualified Data.Char as C
import qualified Control.Monad as O
import qualified Data.List as L
import qualified Data.Matrix as X
import qualified Data.Set as S
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Data.Vector as V

parse_int :: P.Parser Int
parse_int = do
  neg <- P.optionMaybe (P.char '-')
  let
    sign = case neg of
      Just _ -> (-1)
      Nothing -> 1
  digits <- P.many1 P.digit
  return (sign * (foldl (\a d -> a * 10 + C.ord d - C.ord '0') 0 digits))

parse_point :: P.Parser (V.Vector Int)
parse_point = do
  result <- P.sepBy1 parse_int (P.char ',')
  P.char '\n'
  return (V.fromList result)

parse_scanner :: P.Parser (S.Set (V.Vector Int))
parse_scanner = do
  P.string "--- scanner "
  parse_int
  P.string " ---\n"
  result <- P.many parse_point
  return (S.fromList result)

parse_input :: P.Parser [S.Set (V.Vector Int)]
parse_input = do
  result <- P.sepBy parse_scanner (P.char '\n')
  P.eof
  return result

rotations :: [X.Matrix Int]
rotations = do
  m0 <- map X.fromLists (L.permutations [[1, 0, 0], [0, 1, 0], [0, 0, 1]])
  entries <- sequence (replicate 3 [1,-1])  
  let result = m0 * X.diagonalList 3 0 entries
  O.guard (X.detLaplace result == 1)
  return result

matches :: S.Set (V.Vector Int) -> S.Set (V.Vector Int)
  -> [(S.Set (V.Vector Int), [Int])]
matches s0 s1 = do
  r <- rotations
  let ns1 = S.map (\v -> X.getCol 1 (r * X.colVector v)) s1
  t <- [V.zipWith (-) v0 v1 | v0 <- S.toList s0, v1 <- S.toList ns1]
  let ns2 = S.map (\v -> V.zipWith (+) t v) ns1
  O.guard (S.size (S.intersection s0 ns2) >= 12)
  return (ns2, V.toList t)

correct :: [(Bool, S.Set (V.Vector Int), [Int])]
  -> [(Bool, S.Set (V.Vector Int), [Int])]
correct xs = let
  goods = map (\(_, s, _) -> s) (filter (\(b, _, _) -> b) xs)
  in map (\(g, s, t) -> if g then (g, s, t) else
    case concat (map (flip matches s) goods) of
      [] -> (g, s, t)
      ((ns, nt):_) -> (True, ns, nt)) xs

manhattan_distance :: [Int] -> [Int] -> Int
manhattan_distance xs ys = sum [abs (x - y) | (x, y) <- zip xs ys]

solve :: [S.Set (V.Vector Int)]
  -> (Int, Int)
solve x = let
  mx = x
  corrections = iterate correct (map (\(i, s) -> if i == 0
    then (True, s, [0, 0, 0])
    else (False, s, [0, 0, 0])) (zip [0..] mx))
  corrected = head [c | (c, nc) <- zip corrections (tail corrections), c == nc]
  beacons = S.unions (map (\(_, b, _) -> b) corrected)
  scanners = map (\(_, _, s) -> s) corrected
  p1 = length beacons
  p2 = maximum [manhattan_distance v0 v1 | v0 <- scanners, v1 <- scanners]
  in (p1, p2)

main :: IO ()
main = do
  input <- P.parseFromFile parse_input "inputs/day19/input"
  case input of
    Left e -> error (show e)
    Right x -> print (solve x)
