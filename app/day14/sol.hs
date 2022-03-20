import qualified Data.List as L (sort)
import qualified Data.Map as M ( empty, findWithDefault, insertWith, Map, toList
                               , singleton)
import Text.Parsec (eof, many)
import Text.Parsec.Char (char, string, upper)
import Text.Parsec.String (Parser, parseFromFile)

data Rule = Rule Char Char Char

parse_rule :: Parser Rule
parse_rule = do
  p1 <- upper
  p2 <- upper
  string " -> "
  r <- upper
  char '\n'
  return (Rule p1 p2 r)
  
parse_input :: Parser (String, [Rule])
parse_input = do
  s <- many upper
  string "\n\n"
  rs <- many parse_rule
  eof
  return (s, rs)

type Polymer = M.Map (Char, Char) Int

modify_polymer :: ((Char, Char), Int) -> Polymer -> Polymer
modify_polymer (cs, i) = M.insertWith (+) cs i

str_to_polymer :: String -> Polymer
str_to_polymer s = foldr modify_polymer M.empty
  (map (\cs -> (cs, 1)) (zip s (tail s)))

step :: [Rule] -> Polymer -> Polymer
step rs p = let
  modifications :: [((Char, Char), Int)]
  modifications = concat (map (\(Rule c1 c2 rep) -> 
    let count = M.findWithDefault 0 (c1, c2) p
    in [((c1, c2), -count), ((c1, rep), count), ((rep, c2), count)]) rs)

  in foldr modify_polymer p modifications

solve :: Int -> (String, [Rule]) -> String
solve n (s, rs) = let
  polymer :: Polymer
  polymer = (iterate (step rs) (str_to_polymer s)) !! n

  freq_map :: M.Map Char Int
  freq_map = foldr (\((c1, c2), f) -> M.insertWith (+) c1 f)
    (M.singleton (last s) 1) (M.toList polymer)

  freqs :: [Int]
  freqs = L.sort (map snd (M.toList freq_map))
  
  in show (last freqs - head freqs)

part1 :: (String, [Rule]) -> String
part1 = solve 10

part2 :: (String, [Rule]) -> String
part2 = solve 40

main :: IO ()
main = do
  input <- parseFromFile parse_input "inputs/day14/input"
  case input of
    Left e -> error (show e)
    Right x -> putStrLn (part2 x)
