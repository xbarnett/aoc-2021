import qualified Data.Set as S (filter, fromList, map, member, Set, size)
import MyUtils (parse_int)
import Text.Parsec (eof, many)
import Text.Parsec.Char (char, oneOf, string)
import Text.Parsec.String (Parser, parseFromFile)

data Paper =
  Paper { paper_width :: Int
        , paper_height :: Int
        , paper_holes :: S.Set (Int, Int)
        }

instance Show Paper where
  show p =
    let
      w = paper_width p
      h = paper_height p
      holes = paper_holes p
    in concat [[if S.member (x, y) holes then '#' else '.'
            | x <- [0..w-1]] ++ "\n" | y <- [0..h-1]]
  
data FoldType = FoldX | FoldY

data Fold =
  Fold { fold_type :: FoldType
       , fold_pos :: Int
       }
  
parse_coord :: Parser (Int, Int)
parse_coord = do
  x <- parse_int
  char ','
  y <- parse_int
  char '\n'
  return (x, y)

parse_fold :: Parser Fold
parse_fold = do
  string "fold along "
  axis <- oneOf "xy"
  char '='
  n <- parse_int
  char '\n'
  return Fold { fold_type = if axis == 'x' then FoldX else FoldY
              , fold_pos = n
              }

parse_input :: Parser (Paper, [Fold])
parse_input = do
  coords <- many parse_coord
  let paper = Paper { paper_width = maximum (map fst coords) + 1
                    , paper_height = maximum (map snd coords) + 1
                    , paper_holes = S.fromList coords
                    }
  char '\n'
  folds <- many parse_fold
  eof
  return (paper, folds)

fold_paper :: Paper -> Fold -> Paper
fold_paper paper fold =
  let w = paper_width paper
      h = paper_height paper
      holes = paper_holes paper
      n = fold_pos fold
  in case fold_type fold of
    FoldX -> paper { paper_width = n
                   , paper_holes = S.filter (\(x, y) -> x /= n)
                                   (S.map (\(x, y) ->
                                             (n - abs (x - n), y)) holes)
                   }
             
    FoldY -> paper { paper_height = n
                   , paper_holes = S.filter (\(x, y) -> y /= n)
                                   (S.map (\(x, y) ->
                                             (x, n - abs (y - n))) holes)
                   }
                                      
part1 :: (Paper, [Fold]) -> String
part1 (paper, folds) = show (S.size (paper_holes (fold_paper paper
                                                  (head folds))))

part2 :: (Paper, [Fold]) -> String
part2 (paper, folds) = show (foldl fold_paper paper folds)

main :: IO ()
main = do
  input <- parseFromFile parse_input "inputs/day13/input"
  case input of
    Left err -> error "parse error"
    Right x -> putStrLn (part2 x)
