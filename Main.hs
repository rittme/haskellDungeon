import Characters
import Room
import GameIO

startGame :: IO()
startGame = opening >> gameLoop player

gameLoop :: Character -> IO b
gameLoop c = randomRoom >>= \r -> playRoom r c >>= \p -> gameLoop p
