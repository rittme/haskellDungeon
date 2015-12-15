module Characters where

data Object = Object {name :: String, damage :: Int}
  deriving(Show)

data Character = Player Int [Object] | Monster String Int Object
  deriving(Show)

life :: Character -> Int
life (Monster _ l _) = l
life (Player l _) = l

attack :: Object -> Character -> Character
attack (Object _ damage) (Monster s life d) = Monster s (life - damage) d
attack (Object _ damage) (Player life bag) = Player (life - damage) bag

testbag :: [Object]
testbag = [Object "Dagger" 10, Object "Shock scroll" 15, Object "Dart" 5]

player = Player 100 testbag

monsters :: [Character]

monsters = [Monster "Dragon" 100 (Object "Fire breath" 30),
            Monster "Goblin" 20 (Object "Spear" 5),
            Monster "Ghost" 40 (Object "Soul touch" 15)]

nameChar :: Character -> String
nameChar (Monster s _ _) = s
nameChar _ = "Player"

-- Player used Dagger: 10 damage
-- Dragon used Fire breath: 30 damage
