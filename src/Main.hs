-- | Main entry point to the application.
module Main where

type Health = Integer

type Damage = Integer

type Name = String

type Coord = (Integer, Integer)

data Starship = Destroyed | Starship {
    health :: Health,
    damage :: Damage,
    name   :: Name,
    coord  :: Coord
} deriving (Show, Eq)

attack :: Starship -> Starship -> Starship
attack Destroyed _ = error "Destoyed ship can`t attack anything!"
attack _ Destroyed = Destroyed
attack a t
    | delta > 0  = Starship {name = name t, damage = damage t, health = delta, coord = coord t}
    | otherwise = Destroyed
    where delta = health t - damage a


distance :: Coord -> Coord -> Integer
distance (x1,y1) (x2,y2) = x1*x2 + y1*y2

-- | The main entry point.
main :: IO ()
main = do
    print $ attack Starship {name = "A1", damage = 200, health = 100, coord = (1, 1)} Destroyed
    print $ distance (2,2) (4,5)

