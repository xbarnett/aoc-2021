command_to_coords :: String -> Int -> (Int, Int)
command_to_coords command x = case command of
  "forward" -> (x, 0)
  "down" -> (0, x)
  "up" -> (0, -x)
  _ -> error "invalid command"

input_to_coords :: String -> [(Int, Int)]
input_to_coords input = [ let [command, x_str] = words line
                              x = read x_str
                          in command_to_coords command x
                        | line <- lines input]

coords_sum :: [(Int, Int)] -> (Int, Int)
coords_sum xs = (sum (map fst xs), sum (map snd xs))

solve :: String -> String
solve input =
  let coords = input_to_coords input
      (x, y) = coords_sum coords
  in show (x * y)

main :: IO ()
main = do
  input <- readFile "inputs/day2/input"
  putStrLn (solve input)
