import Data.List (group, sort)
import Control.Monad.RWS hiding (First)
import System.Random
import System.Random.Stateful (uniformRM, uniformM)

{-
Use the RWS Monad to log the rounds as well as maintaining the state of the
random number generator. Unlike the dice game we don't need the Reader to
provide configuration (the bounds for thhe RNG) since we had to define instances of 
Uniform and UniformRange for our own enumerated type. So we just make Reader unit.

We had to change the signatures of thhe functions to use our type for RWS, but other than 
that the only changes to the code were including tell in gameRound to log the weapons
in the round and changing call in main from evalState to evalRWS
-}

data Weapon = Rock | Paper | Scissors
  deriving (Show, Bounded, Enum, Eq)

data Winner = First | Second | Draw
  deriving (Show, Eq, Ord)

-- r = () since no configuration
-- w is the list of weapon pairs for each round
-- s is the random number generator
type WeaponGame = RWS () [(Weapon, Weapon)] StdGen

winner :: (Weapon, Weapon) -> Winner
winner (Paper, Rock) = First
winner (Scissors, Paper) = First
winner (Rock, Scissors) = First
winner (w1, w2)
  | w1 == w2 = Draw
  | otherwise = Second

instance UniformRange Weapon where
  uniformRM (lo, hi) rng = do
    res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
    pure $ toEnum res

instance Uniform Weapon where
  uniformM rng = uniformRM (minBound, maxBound) rng

randomWeapon :: WeaponGame Weapon
randomWeapon = state uniform

gameRound :: WeaponGame (Weapon, Weapon)
gameRound = 
  (,) <$> randomWeapon <*> randomWeapon >>= \ws -> tell [ws] >> pure ws

game :: Int -> WeaponGame [(Winner, Int)]
game n = counts <$> replicateM n (winner <$> gameRound)
  where
    counts xs = map headLength $ group $ sort xs
    headLength xs@(x:_) = (x, length xs)
    headLength [] = error "unexpected"

main :: IO ()
main = do
  newStdGen >>= print . evalRWS (game 10) () 