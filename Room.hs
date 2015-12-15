module Room where

import Data.Maybe
import Data.Char
import Data.String

import Characters

data Room = Room { description :: String, action :: Action }
  deriving (Show)

data Item = Weapon Object | Healing Int | Empty
  deriving (Show)

data Action = Combat Character | Treasure Item | Damage Int | Choices [String]
  deriving (Show)

rooms :: [Room]
rooms = [
  Room "You wake up in the dungeon cell you have been rotting for ages. Everything sounds suspiciously quiet. You notice the celldoor is open." (Choices ["Go out"]),
  Room "You walk into a room. As soon as you step in you feel the air is loaded with a toxic gas, you try to go out as soon as possible but get lightly poisoned." (Damage 10),
  Room "You enter into a strange room. It's completely different from the rest of the dungeon, contains a magnificent font with healing water." (Treasure (Healing 20)),
  Room "You enter into a dark room. In a corner you see a closed chest. Opening it you see a Shock Scroll." (Treasure (Weapon (Object "Shock Scroll" 15)))
  ]



actionChoices :: Action -> String
actionChoices (Treasure Empty) = "The chest was empty.\n1) Continue\n"
actionChoices (Treasure (Weapon o)) = "You find a " ++ name o ++
                                            "\n1) Take " ++ name o ++
                                            "\n2) Leave " ++ name o ++ "\n"
actionChoices (Treasure (Healing l)) = "You can recover " ++ show l ++ " points of life." ++
                                            "\n1) Heal yourself. \n2) Leave\n"
actionChoices (Damage d) = "1) Take " ++ show d ++ " damage and leave the room.\n"
actionChoices (Choices opts) = unlines $ map (\(x,y) -> show x ++ ") " ++ y)(zip [1..] opts)
actionChoices (Combat m) = "1) Fight " ++ nameChar m

actionEffect :: Int -> Action -> Character -> Character
actionEffect _ (Treasure Empty) p = p
actionEffect 1 (Treasure (Weapon o))  (Player life bag) = Player life (o:bag)
actionEffect 2 (Treasure (Weapon o))  player            = player
actionEffect 1 (Treasure (Healing h)) (Player life bag) = Player (life + h) bag
actionEffect 2 (Treasure (Healing h)) player            = player
actionEffect _ (Damage d)             (Player life bag) = Player (life - d) bag --TODO: Player can die
actionEffect _ (Choices _)            player            = player
actionEffect _ (Combat m)             (Player life bag) = undefined --TODO: combat -- TODO: run

showRoom :: Room -> IO()
showRoom (Room d a) = do
                        putStrLn d
                        putStrLn $ actionChoices a

-- runChoice :: Action -> Character -> Character
-- runChoice action player = do
--                             c <- getLine --TODO: to int
--                             return $ actionEffect (read c) action player
