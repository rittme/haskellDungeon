Haskell Dungeon
===============
Text-based Dungeon Adventure Game
---------------------------------
A terminal based dungeon adventure game.
During the game, the player is in charge of a character that delves through a dungeon, room by room, looking for a way to get out.

Each room will be selected randomly from a list of rooms (or randomly generated), and described textually to the user.
Each room may contain one of different possible game situations, for example:
 - a trap (player looses "life points")
 - a treasure chest (player finds new item)
 - a monster

In the case of a monster room, the game enters in a combat situation:
The combat is composed of many rounds, each rounds the player chooses an action, like attack, use an item or flee.
The monster also attacks the player at each round.
If the monster loose all their life points before the player, the game can continue.
If the player lose all it's life points it have to restart the game.

The player goal is to arrive at the final room and leave the dungeon.
