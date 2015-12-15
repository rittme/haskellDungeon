module Characters where

import Data.List
import Data.Maybe
import Data.Char

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
nameChar (Monster s _ _)  = s
nameChar _                = "Player"

-- Player used Dagger: 10 damage
-- Dragon used Fire breath: 30 damage
-------------------------------------------------------------------------
{-
  ###### FIGHT #######
-}
zipBag :: [Object] -> [(Int, Object)] 
zipBag = zip [1..]

printBag :: [(Int, Object)] -> IO ()
printBag []   = putStrLn "*** No weapon in your bag \n 1) Run " 
printBag list = mapM_ putStrLn [itemToString x | x <- list]

itemToString :: (Int, Object) -> String
itemToString i = show (fst i) ++ ") Attack with " ++ name (snd i)


isIntValue :: String -> Bool
isIntValue value = not (null value) && all isDigit value

getObject :: String -> [(Int, Object)] -> Maybe (Int, Object)
getObject _ []                      = Nothing
getObject chx bag | isIntValue chx  = do
                                        let x = read chx :: Int
                                        find (\ t -> fst t == x) bag
                  | otherwise       = Nothing 
-- The fight is end : The player or the monster is dead
isEndfight :: Character -> Character -> Bool
isEndfight (Player endur _) (Monster _ life _) = endur <= 0 || life <= 0


getChoice :: [Object] -> IO String
getChoice bag = do
                  putStrLn "*** Please choose a weapon ***" 
                  printBag (zipBag bag) 
                  choice <- getLine
                  if isIntValue choice
                    then return choice
                  else getChoice bag


getFightStatus :: Character -> Object -> Character -> [String]
getFightStatus (Player l b) (Object pn pdm) (Monster s life (Object mn mdm)) = 
                ["---------------------------------- \n"
                            ,"Player used " ++ pn ++ ": " ++ show pdm ++ " damage \n"
                            ,s ++ " used " ++ mn ++ ": " ++ show mdm ++ " damage \n"
                            ,"\nPlayer remaining life: " ++ show l ++ " \n"
                            ,s ++ " remaining life: " ++ show life ++ " \n"
                            ,"---------------------------------- \n"]


showStatus :: Character -> Maybe (Int, Object) -> Character -> IO ()
showStatus p o m  | isNothing o  = return ()
                  | otherwise    = mapM_ putStrLn $ 
                          getFightStatus p (snd (fromJust o)) m 


-- TODO : check player's (and monster's ??) life and where
fightRound :: Character -> Maybe (Int, Object) -> Character
fightRound char obj | isNothing obj = char
                    | otherwise     = attack (snd (fromJust obj)) char 

fight :: Character -> Character -> IO Character
fight (Player l b) monster | isEndfight (Player l b) monster = return (Player l b)
                           | otherwise = do 
                                  choice <- getChoice b
                                  let zippedBag = zipBag b
                                  let ob  = getObject choice zippedBag
                                  let p   = fightRound (Player l b) ob
                                  let m   = fightRound monster ob
                                  showStatus p ob m
                                  fight p m
                                  


-------------------------------------------------------------------------
