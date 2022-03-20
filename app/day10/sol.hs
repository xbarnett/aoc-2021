import Data.Either (fromRight)
import Data.Foldable (foldlM)
import Data.List (findIndex, sort)
import Data.Maybe (fromJust)
import Text.Parsec (eof, many)
import Text.Parsec.Char (char, oneOf)
import Text.Parsec.String (Parser, parseFromFile)

brackets :: [String]
brackets = ["()", "[]", "{}", "<>"]

parse_line :: Parser String
parse_line = do
  result <- many (oneOf (concat brackets))
  char '\n'
  return result

parse_input :: Parser [String]
parse_input = do
  result <- many parse_line
  eof
  return result

closing :: Char -> Char
closing c = (brackets !! i) !! 1
  where
    i = fromJust (findIndex (\s -> head s == c) brackets)

add_char :: String -> Char -> Either Char String
add_char s c = if elem c (map head brackets) then Right (c : s) else
  case s of
    "" -> Left c
    h:s -> if c == closing h then Right s else Left c

syntax_check :: String -> Either Char String
syntax_check s = foldlM add_char "" s

score_line :: String -> Int
score_line s = case syntax_check s of
  Right _ -> 0
  Left ')' -> 3
  Left ']' -> 57
  Left '}' -> 1197
  Left '>' -> 25137

part1 :: [String] -> Int
part1 xs = sum (map score_line xs)

score_bracket :: Char -> Int
score_bracket b = case b of
  ')' -> 1
  ']' -> 2
  '}' -> 3
  '>' -> 4

score_completion :: String -> Int
score_completion s = foldl (\x b -> 5 * x + score_bracket b) 0 s

part2_score_line :: String -> Int
part2_score_line s = case syntax_check s of
  Left _ -> 0
  Right s -> score_completion (map closing s)

part2 :: [String] -> Int
part2 xs = scores !! (div (length scores) 2)
  where
    scores = sort (filter (/= 0) (map part2_score_line xs))

main :: IO ()
main = do
  input <- parseFromFile parse_input "inputs/day10/input"
  print (part2 (fromRight (error "parse error") input))
