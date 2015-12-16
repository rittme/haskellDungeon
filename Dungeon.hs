module Dungeon where

import Characters
import Room

dungeon = [Room "You wake up in the dungeon cell you have been rotting for ages.\n\
\Everything sounds suspiciously quiet. You notice the cell door is open."
  (Choices ["Quickly get out.", "Sneak your way out"]),
  Room "As you enter the guards room you see 3 men on the floor. They look dead.\n\
  \As you walk through the room you start to feel dizzy. You notice the air is loaded\n\
  \with toxic gas. You manage to get out, but not without getting lightly poisoned."
    (Damage 10),

  Room "The stairs going to the surface seems blocked by debris. You have only one\n\
  \choice of going further down into this scary place."
    (Choices ["Go down through the dark stairwell."]),

  Room "You walk through some dark corridors, hardly seeing what is in front of you.\n\
  \At some point you see a light coming from a door crack."
    (Choices ["Get inside the room.", "Keep going through the darkness."]),

  Room "You stumble with a group of enraged goblins. You barely have time to reach for\n\
  \your weapon before they jump over you.\n\
  \       ,      ,                 ,      ,\n\
  \      /(.-\"\"-.)\\               /(.-\"\"-.)\\\n\
  \  |\\  \\/      \\/  /|       |\\  \\/      \\/  /|        |\\     ____\n\
  \  | \\ / =.  .= \\ / |       | \\ / =.  .= \\ / |        | \\.-./ .-'\n\
  \  \\( \\   o\\/o   / )/       \\( \\   o\\/o   / )/         \\ _  _(\n\
  \   \\_, '-/  \\-' ,_/         \\_, '-/  \\-' ,_/          | .)(./\n\
  \     /   \\__/   \\             /   \\__/   \\            |   \\(\n\
  \     \\,___/\\___,/             \\,___/\\___,/            |     \\   |\n\
  \   ___\\ \\|uu|/ /___         ___\\ \\|  |/ /___          |  \\vvv   |\n\
  \ /`    \\ .--. /    `\\     /`    \\ .--. /    `\\        |  |__    |\n\
  \/       '----'       \\   /       '----'       \\      /      `-. |"
    (Combat (Monster "Goblins" 40 (Object "Spears and stones" 7))),

  Room "After dealing with those goblins you have the occasion to have a good look at\n\
  \the room. In a corner you find a chest. Looking inside you see a nicely crafted sword.\n\
  \              (O)\n\
  \              <M\n\
  \   o          <M\n\
  \  /| ......  /:M\\------------------------------------------------,,,,,,\n\
  \(O)[]XXXXXX[]I:K+}=====<{H}>================================------------>\n\
  \  \\| ^^^^^^  \\:W/------------------------------------------------''''''\n\
  \   o          <W\n\
  \              <W\n\
  \              (O)"
    (Treasure (Weapon (Object "Sword" 20))),

  Room "You keep dwelling through the dungeon and you find yourself in a strange room.\n\
  \It's clean and all the walls are shiny. At the center it contains a magnificent\n\
  \fountain overflowing with healing water.\n\
  \                     .      .       .       .\n\
  \  .   .       .          .      . .      .         .          .    .\n\
  \         .       .         .    .   .         .         .            .\n\
  \    .   .    .       .         . . .        .        .     .    .\n\
  \ .          .   .       .       . .      .        .  .              .\n\
  \      .  .    .  .       .     . .    .       . .      .   .        .\n\
  \ .   .       .    . .      .    . .   .      .     .          .     .\n\
  \    .            .    .     .   . .  .     .   .               .\n\
  \     .               .  .    .  . . .    .  .                 .\n\
  \                        . .  .  . . .  . .\n\
  \                            . . . . . .\n\
  \                              . . . .\n\
  \                               I . I\n\
  \                 _______________III_______________\n\
  \                /    .       .       .       .    \\\n\
  \               ( ~~~ .  ~~~  .  ~~~  .  ~~~  . ~~~ )\n\
  \                 \\SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS/\n\
  \                    \\ ======================= /\n\
  \                        \\SSSSSSSSSSSSSSSSS/\n\
  \                     ________\\       /________\n\
  \                    (=+=+=+=+=+=+=+=+=+=+=+=+=)\n\
  \                     ~~~~~~~~~~~~~~~~~~~~~~~~~"
    (Treasure (Healing 25)),

  Room "Your way leads you to a long corridor. You can feel a breeze coming from it.\n\
  \That's your way out!"
    (Choices ["Run to your freedom!"]),

  Room "After passing by a small door you find yourself outside. It seems you are at the\n\
  \summit of a dormant volcano. There are hot gushes of vapor coming out of holes in\n\
  \the floor. In the center of the crater lies a pile of gold and gems.\n\
  \Standing over it there is a mighty Dragon, king of the mountain.\n\
  \He looks at you with his greedy eyes. If you want to be free, you will have\n\
  \to make your way through him.\n\
  \ <>=======()\n\
  \(/\\___   /|\\\\          ()==========<>_\n\
  \      \\_/ | \\\\        //|\\   ______/ \\)\n\
  \        \\_|  \\\\      // | \\_/\n\
  \          \\|\\/|\\_   //  /\\/\n\
  \           (oo)\\ \\_//  /\n\
  \          //_/\\_\\/ /  |\n\
  \         @@/  |=\\  \\  |\n\
  \              \\_=\\_ \\ |\n\
  \                \\==\\ \\|\\_\n\
  \             __(\\===\\(  )\\\n\
  \            (((~) __(_/   |\n\
  \                 (((~) \\  /\n\
  \                 ______/ /\n\
  \                 '------'"
    (Combat (Monster "Dragon" 150 (Object "Fire breath" 25)))]
