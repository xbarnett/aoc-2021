import qualified Control.Monad.State.Lazy as O
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
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

data Player = Player {
  player_score :: Int
  ,player_position :: Int
  }
  deriving (Eq, Ord)

parse_player :: P.Parser Player
parse_player = do
  P.string "Player "
  parse_int
  P.string " starting position: "
  position <- parse_int
  P.char '\n'
  return Player {
    player_score = 0
    ,player_position = position
    }

data State = State {
  state_players :: [Player]
  ,state_moves :: Int
  }

parse_input :: P.Parser State
parse_input = do
  players <- P.count 2 parse_player
  P.eof
  return State {
    state_players = players
    ,state_moves = 0
    }

move_player :: Player -> Int -> Player
move_player p r = let
  new_pos = mod (player_position p - 1 + r) 10 + 1
  in Player {
    player_score = player_score p + new_pos
    ,player_position = new_pos
    }
     
step :: State -> State
step s = let
  m = state_moves s
  in State {
    state_players = map (\(i, p) -> if i == mod m 2
      then move_player p (9*m+6) else p) (zip [0,1] (state_players s))
    ,state_moves = m + 1
    }

play :: State -> Int
play is = let
  fs = head (filter (\s -> any (\p -> player_score p >= 1000)
                      (state_players s)) (iterate step is))
  losing_score = head (filter (< 1000) (map player_score (state_players fs)))
  in losing_score * state_moves fs * 3

rolls :: [(Int,Int)]
rolls = let
  scores = map sum (sequence (replicate 3 [1,2,3]))
  in map (\g -> (head g, length g)) (L.group (L.sort scores))

has_won :: Player -> Bool
has_won p = player_score p >= 21

futures :: (Player, Player)
  -> O.State (M.Map (Player, Player) (Int, Int)) (Int, Int)
futures (p1, p2) = if has_won p1 then return (1, 0) else
  if has_won p2 then return (0, 1) else do
    m0 <- O.get
    case M.lookup (p1, p2) m0 of
      Nothing -> do
        let next_players = [(p2, move_player p1 r) | (r, _) <- rolls]
        next_futures0 <- sequence (map futures next_players)
        let next_futures1 = map (\((f1, f2), c) -> (f1 * c, f2 * c))
              (zip next_futures0 (map snd rolls))
            result = (sum (map snd next_futures1), sum (map fst next_futures1))
        m1 <- O.get
        O.put (M.insert (p1, p2) result m1)
        return result
      Just result -> return result
            
solve :: State -> (Int, Int)
solve x = let
  part1 = play x
  [p1, p2] = state_players x
  (w1, w2) = O.evalState (futures (p1, p2)) M.empty
  part2 = max w1 w2
  in (part1, part2)

main :: IO ()
main = do
  input <- P.parseFromFile parse_input "inputs/day21/input"
  case input of
    Left e -> error (show e)
    Right x -> print (solve x)
