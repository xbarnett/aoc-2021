import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.SP as G
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.String as P

parse_digit :: P.Parser Int
parse_digit = do
  d <- P.digit
  return (C.ord d - C.ord '0')

parse_row :: P.Parser (M.Map Int Int)
parse_row = do
  digits <- P.many parse_digit
  P.char '\n'
  return (M.fromList (zip [0..] digits))

parse_input :: P.Parser (M.Map (Int, Int) Int)
parse_input = do
  rows <- P.many parse_row
  P.eof
  return (M.unions (map (\(i, m') -> M.mapKeys (\j -> (i, j)) m')
                    (zip [0..] rows)))

get_dimensions :: M.Map (Int, Int) Int -> (Int, Int)
get_dimensions m = (maximum (map fst (M.keys m)) + 1
                   ,maximum (map snd (M.keys m)) + 1)

get_graph :: M.Map (Int, Int) Int -> (G.Gr (Int, Int) Int, G.Node, G.Node)
get_graph m = let
  w, h :: Int
  (w, h) = get_dimensions m
                       
  encode_node :: (Int, Int) -> Int
  encode_node (x, y) = x * w + y

  nodes :: [G.LNode (Int, Int)]
  nodes = map (\c -> (encode_node c, c)) (M.keys m)

  adjacents :: (Int, Int) -> [(Int, Int)]
  adjacents (x, y) = filter (\(i, j) -> 0 <= i && i < w && 0 <= j && j < h)
    [(x-1, y), (x, y-1), (x, y+1), (x+1, y)]

  edges :: [G.LEdge Int]
  edges = concat (map (\p -> map (\a -> (encode_node a, encode_node p, m M.! p))
                        (adjacents p)) (M.keys m))
          
  in (G.mkGraph nodes edges, encode_node (0, 0), encode_node (w-1, h-1))

part1 :: M.Map (Int, Int) Int -> Int
part1 m = let (g, s, d) = get_graph m
  in case G.spLength s d g of
    (Just x) -> x
    Nothing -> error "no path found"

expand_map :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
expand_map m = let
  w, h :: Int
  (w, h) = get_dimensions m
  
  replicas :: ((Int, Int), Int) -> [((Int, Int), Int)]
  replicas ((x, y), v) = [((x+i*w, y+j*h), mod (v+i+j-1) 9 + 1)
                         | i <- [0..4], j <- [0..4]]

  in M.unions (map (M.fromList . replicas) (M.assocs m))

part2 :: M.Map (Int, Int) Int -> Int
part2 = part1 . expand_map

main :: IO ()
main = do
  input <- P.parseFromFile parse_input "inputs/day15/input"
  case input of
    Left e -> error (show e)
    Right x -> print (part1 x, part2 x)
