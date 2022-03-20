import MyUtils
import Text.Parsec (eof, many, sepBy)
import Text.Parsec.Char (char, string)
import Text.Parsec.String (parseFromFile, Parser)

parse_input :: Parser [Int]
parse_input = do
  result <- sepBy parse_int (char ',')
  char '\n'
  eof
  return result

part1_fuel :: Int -> [Int] -> Int
part1_fuel pos xs = sum [abs (i - pos) | i <- xs]

part2_fuel :: Int -> [Int] -> Int
part2_fuel pos xs = sum [let dist = abs (i - pos) in div (dist * (dist + 1)) 2 | i <- xs]

solve :: (Int -> [Int] -> Int) -> [Int] -> Int
solve fuel xs = minimum [fuel pos xs | pos <- [minimum xs..maximum xs]]

part1 :: [Int] -> Int
part1 = solve part1_fuel

part2 :: [Int] -> Int
part2 = solve part2_fuel      

main :: IO ()
main = do
  result <- parseFromFile parse_input "inputs/day7/input"
  case result of
    Left err -> print err
    Right x -> print (part2 x)
