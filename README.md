Haskell Dungeon
===============
Text-based Dungeon Adventure Game
---------------------------------
A terminal based dungeon adventure game.
During the game, the player is in charge of a character that delves through a dungeon, room by room, looking for a way to get out.

Dungeons can be created using Haskell Dungeon (.hd) files that will be parsed for each game. To see a syntax example, use the sample file: game1.hd

Each room may contain one of different possible game situations, for example:
 - A treasure chest (adds new item to player)
 - A healing treasure
 - A trap
 - a monster

In the case of a monster room, the game enters in a combat situation:
The combat is composed of many rounds. For each round the player can choose from some different actions.
The monster also attacks the player at each round.
If the monster loose all their life points before the player, the game can continue.
If the player lose all it's life points, the game is over.

The player goal is to arrive at the final room and leave the dungeon.

Launch the game
-----------
To launch the game you have to load the HaskellDungeon.hs module and then run the function `startGame <game_file_name.hd>`
