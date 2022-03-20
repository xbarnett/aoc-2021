import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

data Packet = Packet {
   packet_version :: Int
  ,packet_type :: Int
  ,packet_value :: Int
  ,packet_subs :: [Packet]
  }
  deriving (Show)

bin_to_int :: String -> Int
bin_to_int s = foldl (\x b -> if b == '0' then 2*x else 2*x+1) 0 s

parse_bit :: P.Parser Char
parse_bit = P.oneOf "01"

parse_value :: P.Parser String
parse_value = do
  prefix <- parse_bit
  group <- P.count 4 parse_bit
  if prefix == '0' then return group else do
    rem <- parse_value
    return (group ++ rem)

parse_sub_packets :: P.Parser [Packet]
parse_sub_packets = do
  mode <- parse_bit
  if mode == '0'
    then do
    bit_length <- P.count 15 parse_bit
    sub_packet_bits <- P.count (bin_to_int bit_length) parse_bit
    case P.parse (P.many parse_packet) "" sub_packet_bits of
      Left e -> error (show e)
      Right ps -> return ps
    else do
    sub_packet_length <- P.count 11 parse_bit
    P.count (bin_to_int sub_packet_length) parse_packet

parse_packet :: P.Parser Packet
parse_packet = do
  version_bits <- P.count 3 parse_bit
  type_bits <- P.count 3 parse_bit
  value_digits <- if bin_to_int type_bits == 4 then parse_value
    else (return "")
  sub_packets <- if bin_to_int type_bits /= 4 then parse_sub_packets
    else (return [])
  return Packet {
     packet_version = bin_to_int version_bits
    ,packet_type = bin_to_int type_bits
    ,packet_value = bin_to_int value_digits
    ,packet_subs = sub_packets
    }

parse_outer_packet :: P.Parser Packet
parse_outer_packet = do
  p <- parse_packet
  P.many (P.char '0')
  P.eof
  return p

hex_to_bits :: Char -> String
hex_to_bits h = case h of
  '0' -> "0000"
  '1' -> "0001"
  '2' -> "0010"
  '3' -> "0011"  
  '4' -> "0100"
  '5' -> "0101"
  '6' -> "0110"
  '7' -> "0111"  
  '8' -> "1000"
  '9' -> "1001"
  'A' -> "1010"
  'B' -> "1011"  
  'C' -> "1100"
  'D' -> "1101"
  'E' -> "1110"
  'F' -> "1111"  

parse_input :: P.Parser Packet
parse_input = do
  hexes <- P.many (P.oneOf (['0'..'9'] ++ ['A'..'F']))
  P.char '\n'
  P.eof
  case P.parse parse_outer_packet "" (concat (map hex_to_bits hexes)) of
    Left e -> error (show e)
    Right p -> return p

version_sum :: Packet -> Int
version_sum p = packet_version p + sum (map version_sum (packet_subs p))

part1 :: Packet -> Int
part1 = version_sum

eval :: Packet -> Int
eval p = let s = map eval (packet_subs p) in case packet_type p of
  0 -> sum s
  1 -> product s
  2 -> minimum s
  3 -> maximum s
  4 -> packet_value p
  5 -> if s !! 0 > s !! 1 then 1 else 0
  6 -> if s !! 0 < s !! 1 then 1 else 0
  7 -> if s !! 0 == s !! 1 then 1 else 0 

part2 :: Packet -> Int
part2 = eval

main :: IO ()
main = do
  input <- P.parseFromFile parse_input "inputs/day16/input"
  case input of
    Left e -> error (show e)
    Right x -> print (part1 x, part2 x)
