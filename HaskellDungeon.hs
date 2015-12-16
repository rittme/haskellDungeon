module HaskellDungeon where

import Helpers
import Characters
import Room
import GameIO
import DungeonParser

-- List of game characters
characters = [
  ("The strong warrior and his powerful axe.",
    Player 120 [Object "Axe" 20]),
  ("The mysterious wizard and his spells.",
    Player 60 [Object "Shock Spell" 40]),
  ("The agile rogue and his dagger.",
    Player 80 [Object "Dagger" 30])]

-- Choose the character we want to play with
chooseCharacter :: IO Character
chooseCharacter = do
                    putStrLn "Choose you character wisely:\n"
                    c <- promptInt (unlines $ map (\(x,y) -> show x ++ ") "
                      ++ fst y)(zip [1..] characters)) 1 (length characters)
                    return $ snd (characters !! (c - 1))

-- Start the main game loop from a dungeon file
startGame :: FilePath -> IO()
startGame config = do
                    opening
                    player <- chooseCharacter
                    dungeon <- getDungeon config
                    gameLoop dungeon player

-- Main game loop
gameLoop :: [Room] -> Character -> IO()
gameLoop [] c = showVictory
gameLoop (r:rs) c | amIAlive c = printPlayer c >> playRoom r c >>=
                                   \p -> gameLoop rs p
                  | otherwise  = gameOver

