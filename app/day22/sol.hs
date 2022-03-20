import qualified Data.Char as C
import qualified Data.Set as S
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

parse_int :: P.Parser Int
parse_int = do
  neg <- P.optionMaybe (P.char '-')
  let
    sign = case neg of
      Just _ -> (-1)
      Nothing -> 1
  digits <- P.many1 P.digit
  return (sign * (foldl (\a d -> a * 10 + C.ord d - C.ord '0') 0 digits))

data Step = Step {
  step_on :: Bool
  ,step_x0 :: Int
  ,step_x1 :: Int
  ,step_y0 :: Int
  ,step_y1 :: Int
  ,step_z0 :: Int
  ,step_z1 :: Int  
  }
  deriving (Show)

parse_step :: P.Parser Step
parse_step = do
  instruction <- (P.try (P.string "on")) P.<|> (P.string "off")
  P.string " x="
  x0 <- parse_int
  P.string ".."
  x1 <- parse_int
  P.string ",y="
  y0 <- parse_int
  P.string ".."
  y1 <- parse_int
  P.string ",z="
  z0 <- parse_int
  P.string ".."
  z1 <- parse_int
  P.char '\n'
  return Step {
    step_on = instruction == "on"
    ,step_x0 = x0
    ,step_x1 = x1
    ,step_y0 = y0
    ,step_y1 = y1
    ,step_z0 = z0
    ,step_z1 = z1
    }

parse_input :: P.Parser [Step]
parse_input = do
  result <- P.many parse_step
  P.eof
  return result

solve :: [Step] -> (Int, Int)
solve x = let
  in (part1 x, 0)

main :: IO ()
main = do
  input <- P.parseFromFile parse_input "inputs/day22/input1"
  case input of
    Left e -> error (show e)
    Right x -> print (solve x)
