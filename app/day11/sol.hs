import Data.Char (ord)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Text.Parsec (eof, many)
import Text.Parsec.Char (char, digit)
import Text.Parsec.String (Parser, parseFromFile)

parse_digit :: Parser Int
parse_digit = do
  d <- digit
  return (ord d - ord '0')

parse_line :: Parser [Int]
parse_line = do
  result <- many parse_digit
  char '\n'
  return result

parse_input :: Parser [[Int]]
parse_input = do
  result <- many parse_line
  eof
  return result

type Point = (Int, Int)

hlookup :: [[a]] -> Point -> a
hlookup b (x, y) = b !! x !! y

solve :: [[Int]] -> (Int, Int)
solve board = (snd (steps !! 100),
                length (takeWhile (\(v, nv) -> nv - v /= w * h)
                [(v, nv) | (v, nv) <- zip fs (tail fs)]) + 1)
  where
    w, h :: Int
    w = length board
    h = length (head board)

    adjacents :: Point -> [Point]
    adjacents (x, y) = filter (\(i, j) -> 0 <= i && i < w && 0 <= j && j < h)
      [(x+i,y+j) | i <- [-1..1], j <- [-1..1],
        i /= 0 || j /= 0]

    substep :: [[(Int, Bool)]] -> [[(Int, Bool)]]
    substep b = [[if will_flash v f then (v, True)
                   else (v + nearby_flashes (x, y), f)
                   | ((v, f), y) <- zip vs [0..]] | (vs, x) <- zip b [0..]]
      where
        will_flash :: Int -> Bool -> Bool
        will_flash v f = not f && v > 9
        
        nearby_flashes :: Point -> Int
        nearby_flashes p = length (filter
          (\q -> uncurry will_flash (hlookup b q)) (adjacents p))

    step :: ([[Int]], Int) -> ([[Int]], Int)
    step (b, f) = ((map (map (\(v, f) -> if f then 0 else v))) final_substep,
                   f + length (filter snd (concat final_substep)))
      where
        substeps :: [[[(Int, Bool)]]]
        substeps = iterate substep (map (map (\v -> (v+1, False))) b)

        final_substep :: [[(Int, Bool)]]
        final_substep = head [s | (s, ns) <- zip substeps (tail substeps),
                               s == ns]

    steps :: [([[Int]], Int)]
    steps = iterate step (board, 0)

    fs :: [Int]
    fs = map snd steps

main :: IO ()
main = do
  input <- parseFromFile parse_input "inputs/day11/input"
  print (solve (fromRight (error "parse error") input))
