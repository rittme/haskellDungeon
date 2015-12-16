module DungeonParser where 

import Parsing


data Action = Treasure Int | Damage Int
  deriving (Show)

damage = do 
   mapM_ char "DAMAGE "
   dm <- myParser
   return $ Damage dm


healing = do
  mapM_ char "HEALING "
  hl <- myParser
  return $ Treasure hl

myParser :: Parser Int
myParser = do 
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
