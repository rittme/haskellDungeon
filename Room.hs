import Data.String

data Room = Room { description :: String, action :: Action }
  deriving (Show)

data Action = Combat | Treasure | Damage Int | Choices { opt :: [String]}
  deriving (Show)

rooms :: [Room]
rooms = [
  Room "You wake up in the dungeon cell you have been rotting for ages. Everything sounds suspiciously quiet. You notice the celldoor is open." (Choices ["Go out"]),
  Room "You walk into a room. As soon as you step in you feel the air is loaded with a toxic gas, you try to go out as soon as possible but get lightly poisoned." (Damage 10)]
