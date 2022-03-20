import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

parse_line :: P.Parser String
parse_line = do
  result <- P.many (P.oneOf ".#")
  P.char '\n'
  return result

parse_input :: P.Parser (String, M.Map (Int, Int) Char)
parse_input = do
  algorithm <- parse_line
  P.char '\n'
  image_lines <- P.many parse_line
  P.eof
  return (algorithm, M.fromList[((i, j), image_lines !! i !! j)
                               | i <- [0..(length image_lines - 1)],
                                 j <- [0..(length (image_lines !! i) - 1)]])

enhance_pixel :: String -> [Char] -> Char
enhance_pixel alg nbs = let
  i = foldl (\a c -> 2 * a + if c == '.' then 0 else 1) 0 nbs
  in alg !! i

get_neighbours :: (Int, Int) -> [(Int, Int)]
get_neighbours (x, y) = [(x+i, y+j) | i <- [-1,0,1], j <- [-1,0,1]]

enhance :: String -> (Char, M.Map (Int, Int) Char)
 -> (Char, M.Map (Int, Int) Char)
enhance alg (def, img) = let
  neighbours = concat (map get_neighbours (M.keys img))
  in
  (enhance_pixel alg (replicate 9 def)
  ,M.fromList (map (\p -> (p, enhance_pixel alg (map
     (\c -> M.findWithDefault def c img) (get_neighbours p)))) neighbours))

repeat_enhance :: String -> M.Map (Int, Int) Char -> Int -> Int
repeat_enhance alg img rep = length (filter (== '#')
         (M.elems (snd ((iterate (enhance alg) ('.', img)) !! rep))))

solve :: (String, M.Map (Int, Int) Char)
  -> (Int, Int)
solve (alg, img) = let
  p1 = repeat_enhance alg img 2
  p2 = repeat_enhance alg img 50
  in (p1, p2)

main :: IO ()
main = do
  input <- P.parseFromFile parse_input "inputs/day20/input"
  case input of
    Left e -> error (show e)
    Right x -> print (solve x)
