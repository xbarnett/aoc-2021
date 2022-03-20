import MyUtils
import Text.Parsec (eof, many)
import Text.Parsec.Char (char, string)
import Text.Parsec.String (parseFromFile, Parser)

parse_line :: Parser (Int, Int, Int, Int)
parse_line = do
  x1 <- parse_int
  char ','
  y1 <- parse_int
  string " -> "
  x2 <- parse_int
  char ','
  y2 <- parse_int
  char '\n'
  return (x1, y1, x2, y2)

parse_input :: Parser [(Int, Int, Int, Int)]
parse_input = do
  result <- many parse_line
  eof
  return result

collinear :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
collinear (xa, ya) (xb, yb) (xc, yc) = (xb - xa) * (yc - ya) == (xc - xa) * (yb - ya)

between :: Int -> Int -> Int -> Bool
between x1 x x2 = (x1 <= x && x <= x2) || (x2 <= x && x <= x1)

point_on_line :: (Int, Int) -> (Int, Int, Int, Int) -> Bool
point_on_line (x, y) (x1, y1, x2, y2) = collinear (x1, y1) (x, y) (x2, y2) && between x1 x x2 && between y1 y y2

solve :: [(Int, Int, Int, Int)] -> Int
solve ls = length [() | x <- [0..max_x], y <- [0..max_y],
                   length (filter (point_on_line (x,y)) ls) >= 2]
  where
    max_x = foldr max 0 [max x1 x2 | (x1, _, x2, _) <- ls]
    max_y = foldr max 0 [max y1 y2 | (_, y1, _, y2) <- ls]    

main :: IO ()
main = do
  result <- parseFromFile parse_input "inputs/day5/input"
  case result of
    Left err -> print err
    Right x -> print (solve x)
