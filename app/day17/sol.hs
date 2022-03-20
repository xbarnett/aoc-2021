import qualified Data.List as L
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

parse_int :: P.Parser Int
parse_int = do
  neg <- P.optionMaybe (P.char '-')
  let sign = case neg of
        Just _ -> (-1)
        Nothing -> 1
  digits <- P.many1 P.digit
  return (sign * read digits)

parse_input :: P.Parser (Int, Int, Int, Int)
parse_input = do
  P.string "target area: x="
  x1 <- parse_int
  P.string ".."
  x2 <- parse_int
  P.string ", y="
  y1 <- parse_int
  P.string ".."
  y2 <- parse_int
  P.char '\n'
  P.eof
  return (x1, x2, y1, y2)

step :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
step (x, y, vx, vy) = (x+vx, y+vy, vx - signum vx, vy-1)

solve :: (Int, Int, Int, Int) -> (Int, Int)
solve (x1, x2, y1, y2) = let
  traj :: Int -> Int -> [(Int, Int)]
  traj vx0 vy0 = takeWhile (\(x, y) -> x <= x2 && y >= y1)
    (map (\(x, y, _, _) -> (x, y)) (iterate step (0, 0, vx0, vy0)))
  
  good_vel :: Int -> Int -> Bool
  good_vel vx0 vy0 = any (\(x, y) -> x1 <= x && x <= x2 && y1 <= y && y <= y2)
    (traj vx0 vy0)

  good_vels :: [(Int, Int)]
  good_vels =  [(vx, vy) | vx <- [0..x2], vy <- [y1..(-y1)], good_vel vx vy]

  max_height :: Int
  max_height = maximum (map snd (traj 0 (maximum (map snd good_vels))))

  distinct_good_vels :: Int
  distinct_good_vels = length (L.nub good_vels)

  in (max_height, distinct_good_vels)

main :: IO ()
main = do
  input <- P.parseFromFile parse_input "inputs/day17/input"
  case input of
    Left e -> error (show e)
    Right x -> print (solve x)
