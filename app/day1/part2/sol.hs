main :: IO ()
main = do
  input <- readFile "inputs/day1/input"
  putStrLn (show (solve (input_to_xs input)))

input_to_xs :: String -> [Int]
input_to_xs input = [read line | line <- lines input]

sum_tuple :: (Int, Int, Int) -> Int
sum_tuple (a, b, c) = a + b + c

solve :: [Int] -> Int
solve xs = length (filter (uncurry (<)) (zip ws (tail ws)))
  where
    ws = map sum_tuple (zip3 xs (tail xs) (tail (tail xs)))
