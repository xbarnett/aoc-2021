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

get_counts :: [Int] -> [Int]
get_counts xs = [length (filter (== i) xs) | i <- [0..8]]

simulate_day :: [Int] -> [Int]
simulate_day xs = [if i == 6 then xs !! 7 + xs !! 0
                   else xs !! (mod (i + 1) 9)
                  | i <- [0..8]]

solve :: Int -> [Int] -> Int
solve steps xs = sum (iterate simulate_day (get_counts xs) !! steps)

part1 :: [Int] -> Int
part1 = solve 80

part2 :: [Int] -> Int
part2 = solve 256

main :: IO ()
main = do
  result <- parseFromFile parse_input "inputs/day6/input"
  case result of
    Left err -> print err
    Right x -> print (part2 x)
