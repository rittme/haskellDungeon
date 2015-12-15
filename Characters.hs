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
nameChar (Monster s _ _)   = s
nameChar _           = "Player"

-- Player used Dagger: 10 damage
-- Dragon used Fire breath: 30 damage

zipBag :: [Object] -> [(Int, Object)] 
zipBag list   = zip [1..] list

printBag :: [(Int, Object)] -> IO ()
printBag []     = putStrLn "*** No weapon in your bag \n 1) Run "
printBag (list)   = mapM_ putStrLn [itemToString x | x <- list]

itemToString :: (Int, Object) -> String
itemToString i = show (fst i) ++ ") Attack with " ++ (name (snd i))


isIntValue :: [Char] -> Bool
isIntValue value = length value > 0 && all isDigit value

getObject :: String -> [(Int, Object)] -> Maybe (Int, Object)
getObject _ []             = Nothing
getObject chx bag | isIntValue chx  = do
                    let x = read chx :: Int
                    object <- find (\t -> fst t == x) bag
                    return object
                  | otherwise     = Nothing

-- the player win the fight
win :: Character -> Character -> Bool
win (Player endurance _) (Monster _ life _) = endurance > 0 && life <= 0

-- the player lost the fight (died)
lost :: Character -> Bool
lost (Player endurance _) = endurance <= 0



{- fight :: Character -> Character -> IO ()
fight (Player l b) monster | win (Player l b) monster = putStrLn "you win !" -- to improve
                           | lost (Player l b)        = putStrLn "you lost !"
                           | otherwise = do
                                  printBag (zipBag b)
                                  choice <- getLine

                                  fight (battle choice b (Player l b)) (battle choice b monster)


-- TODO : check player's (and monster's ??) life and where
fightRound :: Character -> Maybe (Int, Object) -> Character
fightRound char obj | isNothing (obj) = char
                    | otherwise       = attack (snd obj) char

battle:: String -> [Object] -> Character -> Character
battle choice bag (Player l b) = do 
                                    zippedBag <- zipBag bag
                                    ob <- getObject choice zippedBag
                                    p  <- fightRound (Player l b) ob
                                    return p

battle choice bag (Monster s life d) = do 
                                    zippedBag <- zipBag bag
                                    ob <- getObject choice zippedBag
                                    m  <- fightRound (Monster s life d) ob
                                    return m -}


