import MyUtils

gamma_rate :: [String] -> String
gamma_rate nums = [ mode (map (!! i) nums)
                  | i <- [0 .. (length (nums !! 0) - 1)]]

epsilon_rate :: [String] -> String
epsilon_rate nums = [ unmode (map (!! i) nums)
                    | i <- [0 .. (length (nums !! 0) - 1)]]

dig_to_dec :: Char -> Int
dig_to_dec '0' = 0
dig_to_dec '1' = 1
dig_to_dec _ = error "invalid digit"

bin_to_dec :: String -> Int
bin_to_dec xs = sum [(2^(i-1)) * dig_to_dec (xs !! (length xs - i)) | i <- [1 .. (length xs)]]

solve :: String -> String
solve input = show result
  where
    nums = lines input
    gamma = gamma_rate nums
    epsilon = epsilon_rate nums
    result = (bin_to_dec gamma) * (bin_to_dec epsilon)

main :: IO ()
main = do
  input <- readFile "inputs/day3/input"
  putStrLn (solve input)
