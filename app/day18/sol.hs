import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

data SNum = Num Int | Pair SNum SNum
  deriving (Eq, Show)

parse_int :: P.Parser Int
parse_int = do
  neg <- P.optionMaybe (P.char '-')
  let sign = case neg of
        Just _ -> (-1)
        Nothing -> 1
  digits <- P.many1 P.digit
  return (sign * read digits)

parse_snum :: P.Parser SNum
parse_snum = do
  mx <- P.optionMaybe parse_int
  case mx of
    Nothing -> do
      P.char '['
      n1 <- parse_snum
      P.char ','
      n2 <- parse_snum
      P.char ']'
      return (Pair n1 n2)
    Just x -> return (Num x)

parse_line :: P.Parser SNum
parse_line = do
  result <- parse_snum
  P.char '\n'
  return result

parse_input :: P.Parser [SNum]
parse_input = do
  result <- P.many parse_line
  P.eof
  return result

rightmost :: SNum -> [Int]
rightmost (Num _) = []
rightmost (Pair s0 s1) = 1 : rightmost s1

next_left :: SNum -> [Int] -> Maybe [Int]
next_left (Num _) _ = Nothing
next_left _ [] = Nothing
next_left (Pair s0 s1) (x:xs) =
  case next_left (if x == 0 then s0 else s1) xs of
    Nothing -> case x of
      0 -> Nothing
      1 -> Just (0 : rightmost s0)
    Just p -> Just (x:p)

leftmost :: SNum -> [Int]
leftmost (Num _) = []
leftmost (Pair s0 s1) = 0 : leftmost s0

next_right :: SNum -> [Int] -> Maybe [Int]
next_right (Num _) _ = Nothing
next_right _ [] = Nothing
next_right (Pair s0 s1) (x:xs) =
  case next_right (if x == 0 then s0 else s1) xs of
    Nothing -> case x of
      0 -> Just (1 : leftmost s1)
      1 -> Nothing
    Just p -> Just (x:p)

retrieve :: SNum -> [Int] -> SNum
retrieve s [] = s
retrieve (Pair s0 s1) (c:cs) = case c of
  0 -> retrieve s0 cs
  1 -> retrieve s1 cs

modify :: SNum -> [Int] -> SNum -> SNum
modify _ [] n = n
modify (Pair s0 s1) (c:cs) n = case c of
  0 -> Pair (modify s0 cs n) s1
  1 -> Pair s0 (modify s1 cs n)

find_nested_pair :: Int -> SNum -> Maybe [Int]
find_nested_pair _ (Num _) = Nothing
find_nested_pair 0 (Pair _ _) = Just []
find_nested_pair n (Pair s0 s1) = case find_nested_pair (n-1) s0 of
  Nothing -> case find_nested_pair (n-1) s1 of
    Nothing -> Nothing
    Just c -> Just (1:c)
  Just c -> Just (0:c)

explode :: SNum -> SNum
explode s = case find_nested_pair 4 s of
  Nothing -> s
  Just pc -> let
    lc = next_left s pc
    rc = next_right s pc
    (Pair (Num p0) (Num p1)) = retrieve s pc
    m1 = case lc of
      Nothing -> s
      Just c -> let
        (Num l) = retrieve s c
        in modify s c (Num (l + p0))
    m2 = case rc of
      Nothing -> m1
      Just c -> let
        (Num r) = retrieve s c
        in modify m1 c (Num (r + p1))
    in modify m2 pc (Num 0)

find_ge10 :: SNum -> Maybe [Int]
find_ge10 (Num x) = if x >= 10 then Just [] else Nothing
find_ge10 (Pair s0 s1) = case find_ge10 s0 of
  Nothing -> case find_ge10 s1 of
    Nothing -> Nothing
    Just c -> Just (1 : c)
  Just c -> Just (0 : c)

split :: SNum -> SNum
split s = let
  ss = iterate explode s
  m = head [s' | (s', ns) <- zip ss (tail ss), s' == ns]
  in case find_ge10 m of
  Nothing -> m
  Just c -> let
    (Num n) = retrieve m c
    p0 = div n 2
    p1 = if mod n 2 == 0 then p0 else p0 + 1
    in modify m c (Pair (Num p0) (Num p1))

snum_sum :: SNum -> SNum -> SNum
snum_sum a b = let
  ss = iterate split (Pair a b)
  in head [s' | (s', ns) <- zip ss (tail ss), s' == ns]

magnitude :: SNum -> Int
magnitude (Num x) = x
magnitude (Pair s0 s1) = 3 * magnitude s0 + 2 * magnitude s1

solve :: [SNum] -> (Int, Int)
solve ss = let
  p1 = magnitude (foldl1 snum_sum ss)
  p2 = maximum (map magnitude [snum_sum a b | a <- ss, b <- ss, a /= b])
  in (p1, p2)

main :: IO ()
main = do
  input <- P.parseFromFile parse_input "inputs/day18/input"
  case input of
    Left e -> error (show e)
    Right x -> print (solve x)
