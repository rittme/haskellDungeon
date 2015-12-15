module Room where

import Data.String
import Characters

data Room = Room { description :: String, action :: Action }
  deriving (Show)

data Item = Weapon Object | Life Int
  deriving (Show)

data Action = Combat Character | Treasure Item | Damage Int | Choices { opt :: [String]}
  deriving (Show)

rooms :: [Room]
rooms = [
  Room "You wake up in the dungeon cell you have been rotting for ages. Everything sounds suspiciously quiet. You notice the celldoor is open." (Choices ["Go out"]),
  Room "You walk into a room. As soon as you step in you feel the air is loaded with a toxic gas, you try to go out as soon as possible but get lightly poisoned." (Damage 10),
  Room "You enter into a strange room. It's completely different from the rest of the dungeon, contains a magnificent font with healing water." (Treasure (Life 20)),
  Room "You enter into a dark room. In a corner you see a closed chest. Opening it you see a Shock Scroll." (Treasure (Weapon (Object "Shock Scroll" 15)))
  ]

--showRoom :: Room -> IO()
--showRoom (Room d a) = do
--                        putStrLn d
--                        putStrLn actionChoces
--                        choice <- getLine

