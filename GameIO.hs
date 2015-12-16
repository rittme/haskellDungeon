{-
  Functional Programming -- Lab 4 Mini project: Haskell Dungeon
  ** Text-based Dungeon Adventure Game **

  Group 38 - Bernardo Rittmeyer, Modou Cissé
-}

module GameIO where

import System.Console.ANSI
import Characters

colorStrLn ::  ConsoleIntensity -> ColorIntensity -> Color ->
               ColorIntensity -> Color -> String -> IO ()
colorStrLn bold fgi fg bgi bg str = do
  setSGR [SetConsoleIntensity bold,
          SetColor Foreground fgi fg,
          SetColor Background bgi bg]
  putStr str
  setSGR []
  putStrLn ""

colorText :: Color -> String -> IO()
colorText c = colorStrLn NormalIntensity Dull c Dull Black


opening :: IO()
opening =
  colorText Red "                             .xm*f\"\"??T?@hc. \n\
\                          z@\"` '~((!!!!!!!?*m. \n\
\                        z$$$K   ~~(/!!!!!!!!!Mh \n\
\                      .f` \"#$k'`~~\\!!!!!!!!!!!MMc \n\
\                     :\"     f*! ~:~(!!!!!!!!!!XHMk \n\
\                     f      \" %n:~(!!!!!!!!!!!HMMM. \n\
\                    d          X~!~(!!!!!!!X!X!SMMR \n\
\                    M :   x::  :~~!>!!!!!!MNWXMMM@R \n\
\ n                  E ' *  ueeeeiu(!!XUWWWWWXMRHMMM>                :. \n\
\ E%                 E  8 .$$$$$$$$K!!$$$$$$$$&M$RMM>               :\"5 \n\
\z  %                3  $ 4$$$$$$$$!~!*$$$$$$$$!$MM$               :\" ` \n\
\K   \":              ?> # '#$$$$$#~!!!!TR$$$$$R?@MME              z   R \n\
\?     %.             5     ^\"\"\"~~~:XW!!!!T?T!XSMMM~            :^    J \n\
\ \".    ^s             ?.       ~~d$X$NX!!!!!!M!MM             f     :~ \n\
\  '+.    #L            *c:.    .~\"?!??!!!!!XX@M@~           z\"    .* \n\
\    '+     %L           #c`\"!+~~~!/!!!!!!@*TM8M           z\"    .~ \n\
\      \":    '%.         'C*X  .!~!~!!!!!X!!!@RF         .#     + \n\
\        \":    ^%.        9-MX!X!!X~H!!M!N!X$MM        .#`    +\" \n\
\          #:    \"n       'L'!~M~)H!M!XX!$!XMXF      .+`   .z\" \n\
\            #:    \":      R *H$@@$H$*@$@$@$%M~     z`    +\" \n\
\              %:   `*L    'k' M!~M~X!!$!@H!tF    z\"    z\" \n\
\                *:   ^*L   \"k ~~~!~!!!!!M!X*   z*   .+\" \n\
\                  \"s   ^*L  '%:.~~~:!!!!XH\"  z#   .*\" \n\
\                    #s   ^%L  ^\"#4@UU@##\"  z#   .*\" \n\
\                      #s   ^%L           z#   .r\" \n\
\                        #s   ^%.       u#   .r\" \n\
\                          #i   '%.   u#   .@\" \n\
\                            #s   ^%u#   .@\" \n\
\                              #s x#   .*\" \n\
\                               x#`  .@%. \n\
\                             x#`  .d\"  \"%. \n\
\                           xf~  .r\" #s   \"%. \n\
\                     u   x*`  .r\"     #s   \"%.  x. \n\
\                     %Mu*`  x*\"         #m.  \"%zX\" \n\
\                     :R(h x*              \"h..*dN. \n\
\                   u@NM5e#>                 7?dMRMh. \n\
\                 z$@M@$#\"#\"                 *\"\"*@MM$hL \n\
\               u@@MM8*                          \"*$M@Mh. \n\
\             z$RRM8F\"                             \"N8@M$bL \n\
\            5`RM$#                                  'R88f)R \n\
\            'h.$\"                                     #$x* \n\
\\n\
\==========================================================================\n\
\        _____         _       _ _    ____                                 \n\
\       |  |  |___ ___| |_ ___| | |  |    \\ _ _ ___ ___ ___ ___ ___        \n\
\       |     | .'|_ -| '_| -_| | |  |  |  | | |   | . | -_| . |   |       \n\
\       |__|__|__,|___|_,_|___|_|_|  |____/|___|_|_|_  |___|___|_|_|       \n\
\                                                  |___|                   \n\
\==========================================================================\n\
\ Fight your way through the dangers of Haskell and try to get out alive!"

gameOver :: IO()
gameOver = colorText Red "\
\ #####                          #######                      \n\
\#     #   ##   #    # ######    #     # #    # ###### #####  \n\
\#        #  #  ##  ## #         #     # #    # #      #    # \n\
\#  #### #    # # ## # #####     #     # #    # #####  #    # \n\
\#     # ###### #    # #         #     # #    # #      #####  \n\
\#     # #    # #    # #         #     #  #  #  #      #   #  \n\
\ #####  #    # #    # ######    #######   ##   ###### #    # "

showVictory :: IO()
showVictory = colorText Green "\
\          You're finally free again. Until the next time.\n\
\                            .-\"\"\"\"-.\n\
\                           F       `Y\n\
\                          F          Y\n\
\                         I            I\n\
\                          L          J          ##\n\
\                           L        J          ###\n\
\                       #    `-.__.-'          ####\n\
\              _____   ##                 .---#####-...__\n\
\          .--'     `-###          .--..-'    ######     \"\"`---....\n\
\ _____.----.        ###`.._____ .'          #######\n\
\                    ###       /       -.    ####### _.---\n\
\                    ###     .(              #######\n\
\                     #      : `--...        ######\n\
\                     #       `.     ``.     ######\n\
\                           _   :       :.    #####\n\
\                          ( )            )    ###\n\
\                         ,- -.           /    ##\n\
\                        |,   .|          |   .##\n\
\                        || ' |;         |     '\n\
\                     .  ((_, J         <\n\
\                   .'    | | |         |\n\
\                  /      | ; )         |\n\
\                 '       |_ -          ."

printPlayer :: Character -> IO()
printPlayer (Player l o) = colorStrLn BoldIntensity Dull Cyan Dull Black $
                     "\nPLAYER - life: " ++ show l ++ "\nObjects: " ++ show o
