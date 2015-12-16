module DungeonParser where 

import Parsing
import Room
import Characters
import Data.Maybe

{-
COMBAT "Dragon" 99 "Claw" 20
OBJECT "sword" 20
"HEALING 20"
DAMAGE 20
CHOICES "Door 1" "Door 2" "Door 3"
-}

getDungeon :: FilePath -> IO [Room]
getDungeon f =  do
                  str <- readFile f
                  return $ (fst.fromJust) (parse parseDungeon str)

-- Parse dungeon
parseDungeon :: Parser [Room]
parseDungeon = chain parseRoom (char newline)

-- Parse a room
parseRoom :: Parser Room
parseRoom = do
  d <- string
  char newline
  a <- parseAction
  return $ Room d a

-- Parse an action
parseAction :: Parser Action
parseAction = parseDamage +++ parseHealing +++ parseChoices +++
          parseObject +++ parseCombat

-- Parse a damage action
parseDamage :: Parser Action
parseDamage = do
   mapM_ char "DAMAGE "
   dm <- myIntParser
   char newline
   return $ Damage dm

-- Parse an object treasure action
parseObject :: Parser Action
parseObject = do
   mapM_ char "OBJECT "
   name <- string
   char space
   damage <- myIntParser
   char newline
   return $ Treasure (Weapon (Object name damage))

-- Parse a healing action
parseHealing :: Parser Action
parseHealing = do
  mapM_ char "HEALING "
  hl <- myIntParser
  char newline
  return $ Treasure (Healing hl)

-- Parse a combat action
parseCombat :: Parser Action
parseCombat = do
  mapM_ char "COMBAT "
  name <- string
  char space
  life <- myIntParser
  char space
  weapon <- string
  char space
  dm <- myIntParser
  char newline
  return $ Combat (Monster name life (Object weapon dm))

-- Parse a choices action
parseChoices :: Parser Action
parseChoices = do
    mapM_ char "CHOICES "
    chx <- options
    return $ Choices chx

-- Parse options
options :: Parser [String]
options = do
   str <- chain string (char space)
   char newline
   return str

-- Parse string
string :: Parser String
string = do
  char quote
  cs <- zeroOrMore $ sat (/= quote)
  char quote
  return cs

-- parse a 2 digit int
myIntParser :: Parser Int
myIntParser = do
   d1 <- digit
   d2 <- digit
   d3 <- digit
   return $ read [d1,d2,d3]

space = ' '
quote   = '\"'
newline = '\n'
