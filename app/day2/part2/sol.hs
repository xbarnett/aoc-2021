data Sub = Sub { sub_x :: Int
               , sub_y :: Int
               , sub_aim :: Int
               }

initial_sub = Sub { sub_x = 0
                  , sub_y = 0
                  , sub_aim = 0
                  }

data Com = Com { com_type :: String
               , com_x :: Int
               }

com_to_sub :: Sub -> Com -> Sub
com_to_sub sub com = case com_type com of
  "down" -> sub {sub_aim = sub_aim sub + x}
  "up" -> sub {sub_aim = sub_aim sub - x}
  "forward" -> sub { sub_x = sub_x sub + x
                   , sub_y = sub_y sub + (sub_aim sub * x)
                   }
  where
    x = com_x com
  
str_to_com :: String -> Com
str_to_com s = Com { com_type = ctype
                   , com_x = read x_str}
  where
    [ctype, x_str] = words s

solve :: String -> String
solve input = show result
  where
    commands = [str_to_com line | line <- lines input]
    sub = foldl com_to_sub initial_sub commands
    result = sub_x sub * sub_y sub

main :: IO ()
main = do
  input <- readFile "inputs/day2/input"
  putStrLn (solve input)
    
