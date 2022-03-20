import MyUtils

oxygen_bit :: [String] -> Int -> Char
oxygen_bit xs i | c0 < c1 = '1'
                | c0 == c1 = '1'
                | c0 > c1 = '0'
  where
    c0 = length (filter (== '0') (map (!! i) xs))
    c1 = length (filter (== '1') (map (!! i) xs))

co2_bit :: [String] -> Int -> Char
co2_bit xs i | c0 < c1 = '0'
             | c0 == c1 = '0'
             | c0 > c1 = '1'
  where
    c0 = length (filter (== '0') (map (!! i) xs))
    c1 = length (filter (== '1') (map (!! i) xs))

oxygen_generator :: [String] -> Int -> String
oxygen_generator xs i = if length xs == 1 then head xs else oxygen_generator rem (i + 1)
  where
    bit = oxygen_bit xs i
    rem = filter (\s -> (s !! i) == bit) xs

co2_scrubber :: [String] -> Int -> String
co2_scrubber xs i = if length xs == 1 then head xs else co2_scrubber rem (i + 1)
  where
    bit = co2_bit xs i
    rem = filter (\s -> (s !! i) == bit) xs

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
    n1 = oxygen_generator nums 0
    n2 = co2_scrubber nums 0
    result = (bin_to_dec n1) * (bin_to_dec n2)

main :: IO ()
main = do
  input <- readFile "inputs/day3/input"
  putStrLn (solve input)
