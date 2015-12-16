{-
  Functional Programming -- Lab 4 Mini project : Haskell Dungeon
  ** Text-based Dungeon Adventure Game **

  Group 38 - Bernardo Rittmeyer, Modou Cissé
-}
module Characters where

import Data.List
import Data.Maybe
import Data.Char
import Test.QuickCheck
import System.Random

data Object = Object {name :: String, damage :: Int}

instance Show Object where
  show (Object n d) = n ++ " : " ++ show d

data Character = Player Int [Object] | Monster String Int Object
  deriving(Show)

life :: Character -> Int
life (Monster _ l _) = l
life (Player l _) = l

amIAlive :: Character -> Bool
amIAlive c = life c > 0


monsters :: [Character]

monsters = [Monster "Dragon" 100 (Object "Fire breath" 30),
            Monster "Goblin" 20 (Object "Spear" 5),
            Monster "Ghost" 40 (Object "Soul touch" 15)]

nameChar :: Character -> String
nameChar (Monster s _ _)  = s
nameChar _                = "Player"

monsterObject :: Character -> Object
monsterObject (Monster _ _ o) = o

objectDamage :: Object -> Int
objectDamage (Object _ damage) = damage

-------------------------------------------------------------------------
{-
  ###### FIGHT #######
-}

-- attack : a Character (player or monster) attacks the other
--          returns the attacked Character (with the resulted damage)
attack :: Object -> Character -> Double -> Character
attack (Object _ damage) (Monster s life d) coeff =
                  Monster s (life - floor (coeff * fromIntegral damage)) d

attack (Object _ damage) (Player life bag) coeff =
                  Player  (life - floor (coeff * fromIntegral damage)) bag

-- properties attack
prop_attack :: Object -> Character -> Double -> Bool
prop_attack object char coef = life (attack object char coef) == 
            (life char - floor (coef * fromIntegral (objectDamage object)))

prop_attackDamage :: Object -> Character -> Double -> Bool
prop_attackDamage obj char coef = life char > life (attack obj char coef)


-- coeff : Generates a coeff for the damage
coeff :: IO Double
coeff = randomRIO (0.5, 2.0)

prop_coeff :: IO Bool
prop_coeff = do
            c <- coeff
            return $ c >= 0.0 && c <= 2.0

-- zipBag : Zips a bag of object (used to display choices)
zipBag :: [Object] -> [(Int, Object)]
zipBag = zip [1..]

-- printBag : Prints the player's bag of objects
printBag :: [(Int, Object)] -> IO ()
printBag []   = putStrLn "*** No weapon in your bag \n 1) Run "
printBag list = mapM_ putStrLn [itemToString x | x <- list]

    where itemToString :: (Int, Object) -> String
          itemToString i = show (fst i) ++ ") Attack with " ++ name (snd i)


isIntValue :: String -> Bool
isIntValue value = not (null value) && all isDigit value

-- getObject: Get the chosen object from the player's bag
getObject :: String -> [(Int, Object)] -> Maybe (Int, Object)
getObject _ []                      = Nothing
getObject chx bag | isIntValue chx  = do
                                        let x = read chx :: Int
                                        find (\ t -> fst t == x) bag
                  | otherwise       = Nothing

-- isEndfight: The fight is end : The player or the monster is dead
isEndfight :: Character -> Character -> Bool
isEndfight p m = (not . amIAlive) p || (not . amIAlive) m


-- getChoice: get the player's choice
getChoice :: [Object] -> IO String
getChoice bag = do
                  putStrLn "*** Please choose a weapon ***"
                  printBag (zipBag bag)
                  choice <- getLine
                  if isIntValue choice
                    then return choice
                  else getChoice bag

-- fightStart: show the fight starting stats
fightStart :: Character -> Character -> IO()
fightStart (Player pl _) (Monster mn ml _) = putStrLn $ unlines
                                   ["----------------------------------"
                                   ,"Player life: " ++ show pl
                                   ,mn ++ " life: " ++ show ml
                                   ,"---------------------------------- \n"]

-- showStatus : shows the fight status (monster and player information)
showStatus :: Character -> Maybe (Int, Object) -> Character -> 
                                            (Double,Double) -> IO ()
showStatus p o m (cp,co)  | isNothing o  = return ()
                          | otherwise    = putStrLn $ unlines $
                            getStatus p (snd (fromJust o)) m (cp, co)

getStatus :: Character -> Object -> Character -> (Double,Double) -> [String]
getStatus (Player e b) (Object pn pdm) (Monster s l (Object mn mdm)) (cp,cm) =
                    ["----------------------------------"
                      ,"Player used " ++ pn ++ ": " ++ 
                      show (floor (cp * fromIntegral pdm)) ++ " damage"
                      ,s ++ " used " ++ mn ++ ": " ++ 
                      show (floor (cm * fromIntegral mdm)) ++ " damage"
                      ,"Player remaining life: " ++ show e
                      ,s ++ " remaining life: " ++ show l
                      ,"---------------------------------- \n"]

-- playerAttack: the player attacks the monster with th choosen weapon
playerAttack :: Character -> Maybe (Int, Object) -> Double -> Character
playerAttack m obj c  | isNothing obj   = m
                      | otherwise       = attack (snd (fromJust obj)) m c
 

-- fight : A fight between the player and a monster
--          returns the player and the end (one is dead)
fight :: Character -> Character -> IO Character
fight (Player l b) monster | isEndfight (Player l b) monster = 
                                                    return (Player l b)
                           | otherwise = do
                                choice <- getChoice b
                                let zippedBag = zipBag b
                                let ob  = getObject choice zippedBag
                                cp <- coeff
                                cm <- coeff
                                let mObject = monsterObject monster
                                let p = attack mObject (Player l b) cm
                                let m = playerAttack monster ob cp
                                showStatus p ob m (cp,cm)
                                fight p m


-------------------------------------------------------------------------
