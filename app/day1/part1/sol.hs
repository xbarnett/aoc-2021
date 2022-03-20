main :: IO ()
main = do
  input <- readFile "inputs/day1/input"
  putStrLn (show (solve (input_to_xs input)))

input_to_xs :: String -> [Int]
input_to_xs input = [read line | line <- lines input]

solve :: [Int] -> Int
solve xs = length (filter (uncurry (<)) (zip xs (tail xs)))
