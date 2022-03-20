import Data.Char (chr, ord)
import Data.Either (fromRight)
import Data.List (findIndex, permutations, sort)
import Data.Maybe (fromJust)
import Text.Parsec (endBy, eof, many, many1, sepBy)
import Text.Parsec.Char (char, lower, string)
import Text.Parsec.String (Parser, parseFromFile)

parse_word :: Parser String
parse_word = many1 lower

parse_line :: Parser ([String], [String])
parse_line = do
  segments <- endBy parse_word (char ' ')
  string "| "
  outputs <- sepBy parse_word (char ' ')
  char '\n'
  return (segments, outputs)

parse_input :: Parser [([String], [String])]
parse_input = do
  result <- many parse_line
  eof
  return result

part1 :: [([String], [String])] -> Int
part1 xs = length (filter (\w -> elem (length w) [2, 4, 3, 7])
                   (concat (map snd xs)))

correct_segments :: [String]
correct_segments = ["abcefg", "cf", "acdeg", "acdfg", "bcdf"
                   ,"abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

encode_char :: [Int] -> Char -> Char
encode_char p c = chr (ord 'a' + (p !! (ord c - ord 'a')))

encode_string :: [Int] -> String -> String
encode_string p s = sort (map (encode_char p) s)

find_permutation :: [String] -> [Int]
find_permutation segments =
  head [p | p <- permutations [0..6],
        sort (map (encode_string p) segments) == sort correct_segments]

word_to_digit :: [Int] -> String -> Int
word_to_digit p s = fromJust (findIndex (== encode_string p s) correct_segments)

solve_entry :: ([String], [String]) -> Int
solve_entry (segments, outputs) =
  sum [10^(3-i) * word_to_digit (find_permutation segments) (outputs !! i)
      | i <- [0..3]]

part2 :: [([String], [String])] -> Int
part2 xs = sum (map solve_entry xs)

main :: IO ()
main = do
  input <- parseFromFile parse_input "inputs/day8/input"
  print (part2 (fromRight (error "parse error") input))
