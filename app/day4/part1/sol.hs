import Data.List
import MyUtils
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

parse_bingo_row :: Parser [Int]
parse_bingo_row = do
  result <- count 5 (many (char ' ') >> parse_int)
  char '\n'
  return result

parse_bingo_board :: Parser [[Int]]
parse_bingo_board = do
  char '\n'
  result <- count 5 parse_bingo_row
  return result

parse_input :: Parser ([Int], [[[Int]]])
parse_input = do
  moves <- sepBy parse_int (char ',')
  char '\n'
  boards <- many parse_bingo_board
  eof
  return (moves, boards)

subset :: Eq a => [a] -> [a] -> Bool
subset x y = all (\a -> elem a y) x

has_won :: [Int] -> [[Int]] -> Bool
has_won moves board = any (\a -> subset a moves) (rows ++ columns)
  where
    rows = board
    columns = transpose board

solve :: ([Int], [[[Int]]]) -> Int
solve (moves, boards) = score * number_called
  where
    move_count = head [i | i <- [0..], any (has_won (take i moves)) boards]
    winning_board = head [b | b <- boards, has_won (take move_count moves) b]
    score = sum [x | x <- concat winning_board, not (elem x (take move_count moves))]
    number_called = moves !! (move_count - 1)

main :: IO ()
main = do
  result <- parseFromFile parse_input "inputs/day4/input"
  case result of
    Left err -> print err
    Right x -> print (solve x)
