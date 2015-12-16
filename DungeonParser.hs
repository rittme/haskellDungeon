module DungeonParser where 

import Parsing

data Item = Weapon Object | Healing Int
data Action = Treasure Item | Damage Int
  deriving (Show)

damage = do 
   mapM_ char "DAMAGE "
   dm <- myIntParser
   return $ Damage dm


healing = do
  mapM_ char "HEALING "
  hl <- myIntParser
  return $ Treasure (Healing hl)

myIntParser :: Parser Int
myIntParser = do 
   d1 <- digit
   d2 <- digit
   return $ read [d1,d2]



actions :: Parser Action
actions = damage +++ healing 

-- Combat Character | Treasure Item | Damage Int | Choices [String]

{-
COMBAT "Dragon" 100 "Claw" 20
OBJECT "sword" 20
HEALING 20
DAMAGE 20
CHOICES "Door 1" "Door 2" "Door 3"
-}
