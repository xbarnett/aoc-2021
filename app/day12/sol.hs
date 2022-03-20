import Data.Char (isLower)
import Text.Parsec (eof, many)
import Text.Parsec.Char (char, letter)
import Text.Parsec.String (Parser, parseFromFile)

parse_line :: Parser (String, String)
parse_line = do
  w1 <- many letter
  char '-'
  w2 <- many letter
  char '\n'
  return (w1, w2)

parse_input :: Parser [(String, String)]
parse_input = do
  result <- many parse_line
  eof
  return result

is_lower :: String -> Bool
is_lower = all isLower

all_paths :: ([String] -> [String]) -> [String] -> [[String]]
all_paths next_choices hist = let choices = next_choices hist in
  if choices == [] then if head hist == "end" then [hist] else [] else
    concat (map (\n -> all_paths next_choices (n : hist)) choices)

part1 :: [(String, String)] -> Int
part1 g = let
  next_choices :: [String] -> [String]
  next_choices hist = let
    curr :: String
    curr = head hist

    fsts, snds :: [String]
    fsts = map fst (filter (\(a, b) -> b == curr) g)
    snds = map snd (filter (\(a, b) -> a == curr) g)
      
    in if curr == "end" then [] else
    filter (\n -> not (is_lower n && elem n hist)) (fsts ++ snds)

  in length (all_paths next_choices ["start"])

part2 :: [(String, String)] -> Int
part2 g = let
  next_choices :: [String] -> [String]
  next_choices hist = let
    curr :: String
    curr = head hist

    fsts, snds :: [String]
    fsts = map fst (filter (\(a, b) -> b == curr) g)
    snds = map snd (filter (\(a, b) -> a == curr) g)

    has_rep_small :: Bool
    has_rep_small = any (\p -> is_lower p &&
                          length (filter (== p) hist) >= 2) hist
      
    in if curr == "end" then [] else if has_rep_small then 
      filter (\n -> not (is_lower n && elem n hist)) (fsts ++ snds)
      else filter (/= "start") (fsts ++ snds)

  in length (all_paths next_choices["start"])     

main :: IO ()
main = do
  input <- parseFromFile parse_input "inputs/day12/input"
  let sol = case input of
        Left err -> "parse error"
        Right x -> "success: " ++ show (part2 x)
  putStrLn sol
