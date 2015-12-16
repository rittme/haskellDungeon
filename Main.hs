import Helpers
import Characters
import Room
import GameIO


characters = [
  ("The strong warrior and his powerful axe.", Player 120 [Object "Axe" 20]),
  ("The mysterious wizard and his spells.", Player 60 [Object "Shock Spell" 40]),
  ("The agile rogue and his dagger.", Player 80 [Object "Dagger" 30])]

chooseCharacter :: IO Character
chooseCharacter = do
                    putStrLn "Choose you character wisely:\n"
                    c <- promptInt (unlines $ map (\(x,y) -> show x ++ ") " ++ fst y)(zip [1..] characters)) 1 (length characters)
                    return $ snd (characters !! (c - 1))

startGame :: IO()
startGame = opening >> chooseCharacter >>= \player -> gameLoop rooms player

gameLoop :: [Room] -> Character -> IO()
gameLoop [] c = playRoom victory c >> showVictory
gameLoop (r:rs) c | life c > 0 = playRoom r c >>= \p -> gameLoop rs p
                | otherwise  = showDead
