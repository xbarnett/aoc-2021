import Data.Char (ord)
import Data.Either (fromRight)
import Data.List (sortBy)
import Data.Set (fromList, member, Set, singleton, unions)
import qualified Data.Set
import Text.Parsec (digit, eof, many, sepBy)
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser, parseFromFile)

parse_height :: Parser Int
parse_height = do
  d <- digit
  return (ord d - ord '0')

parse_line :: Parser [Int]
parse_line = do
  result <- many parse_height
  char '\n'
  return result

parse_input :: Parser [[Int]]
parse_input = do
  result <- many parse_line
  eof
  return result

type Point = (Int, Int)

hlookup :: [[Int]] -> Point -> Int
hlookup m (x, y) = (m !! x) !! y

all_points :: Int -> Int -> [Point]
all_points w h = [(x,y) | x <- [0..w-1], y <- [0..h-1]]

adjacents :: Int -> Int -> Point -> [Point]
adjacents w h (x, y) = filter (\(i,j) -> 0 <= i && i < w && 0 <= j && j < h)
  [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

part1 :: [[Int]] -> Int
part1 m = sum [hlookup m p + 1 | p <- all_points w h, is_low_point p]
  where
    w = length m
    h = length (head m)

    is_low_point :: Point -> Bool
    is_low_point p = all (hlookup m p <) (map (hlookup m) (adjacents w h p))

part2 :: [[Int]] -> Int
part2 m = product (take 3 (sortBy (flip compare) (map length basins)))
  where
    w = length m
    h = length (head m)

    basin_neighbours :: Point -> Set Point
    basin_neighbours p = fromList (filter (\q -> hlookup m q /= 9)
                                   (p : adjacents w h p))

    basin_step :: Set Point -> Set Point
    basin_step b = unions (Data.Set.map basin_neighbours b)

    build_basin :: Point -> Set Point
    build_basin p = head [b | (b, nb) <- zip bs (tail bs), b == nb]
      where
        bs = iterate basin_step (singleton p) 

    get_basins_acc :: Point -> [Set Point] -> [Set Point]
    get_basins_acc p bs = if any (member p) bs then bs else build_basin p : bs

    basins :: [Set Point]
    basins = foldr get_basins_acc [] (filter (\p -> hlookup m p /= 9)
                                      (all_points w h))

main :: IO ()
main = do
  input <- parseFromFile parse_input "inputs/day9/input"
  print (part2 (fromRight (error "parse error") input))
