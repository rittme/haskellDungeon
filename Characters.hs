data Character = Character {life :: Int, bag :: [Object]}
  deriving(Show)

data Object = Object {name :: String, damage :: Int}
  deriving(Show)

--                     name   life damage
data Monster = Monster String Int Int
  deriving(Show)

attack :: Object -> Monster -> Monster
attack (Object _ damage) (Monster s life d) = Monster s (life - damage) d

monsterAttack :: Monster -> Character -> Character
monsterAttack (Monster _ _ d) (Character l b) = Character (l - d) b


testbag :: [Object]
testbag = [Object "Dagger" 10, Object "Shock scroll" 15, Object "Dart" 5]

player = Character 100 testbag

monsters :: [Monster]

monsters = [Monster "Dragon" 100 30,
            Monster "Goblin" 20 5,
            Monster "Ghost" 40 15]
