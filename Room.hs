module Room where

import Data.String
import Test.QuickCheck

import Helpers
import Characters

-- Room type definition, contains the room description and it's action
data Room = Room { description :: String, action :: Action }
  deriving (Show)

-- Definition of the types of Treasure one can find: weapons, healing or empty
data Item = Weapon Object | Healing Int | Empty
  deriving (Show)

{- Posible actions definition
  Combat:   a combat action that takes the enemy as it's character
  Treasure: a treasure to be collected
  Damage:   damage to be taken by the character
  Choices:  an array of choices the character can make
-}
data Action = Combat Character | Treasure Item | Damage Int | Choices [String]
  deriving (Show)

-- Display the different choices the user have for each action
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

-- Returns the character affected by the changes of the chosen action
actionEffect :: Int -> Action -> Character -> Character
actionEffect _ (Treasure Empty) p = p
actionEffect 1 (Treasure (Weapon o))  (Player life bag) = Player life (o:bag)
actionEffect 2 (Treasure (Weapon o))  player            = player
actionEffect 1 (Treasure (Healing h)) (Player life bag) = Player (life + h) bag
actionEffect 2 (Treasure (Healing h)) player            = player
actionEffect _ (Damage d)             (Player life bag) = Player (life - d) bag --TODO: Player can die
actionEffect _ (Choices _)            player            = player
actionEffect _ (Combat m)             (Player life bag) = error "Combat should never get here!"

-------------------- IO --------------------

-- Prompts the user for the desired choice of action
makeChoice :: Action -> Character -> IO Character
makeChoice (Combat m) p = fightStart p m >> fight p m
makeChoice a p = promptInt (actionChoices a) 1 (actionLgt a) >>= \c ->
                return (actionEffect c a p)
  where actionLgt :: Action -> Int
        actionLgt (Treasure Empty) = 1
        actionLgt (Treasure _)     = 2
        actionLgt (Damage _)       = 1
        actionLgt (Choices n)      = length n

-- Plays the passed room with the passed character
playRoom :: Room -> Character -> IO Character
playRoom r c = putStrLn (description r) >> makeChoice (action r) c


-- Generates a random room
--randomRoom :: IO Room
--randomRoom = (generate . elements) dungeon
--
--roomTestDescriptions :: [String]
--roomTestDescriptions = ["A small room.", "A bright room.",
--                    "A big room.", "A dark room."]
--roomTestActions :: [Action]
--roomTestActions = []
--
