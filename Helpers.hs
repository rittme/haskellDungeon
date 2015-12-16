module Helpers where

import Data.Maybe
import Data.Char

-- Read a numeric and return a Just or Nothing if not numeric
maybeRead :: (Read a, Num a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (all isSpace . snd) . reads

-- Prompt the user for an Int between gte and lte
promptInt :: (Read b, Num b) => String -> Int -> Int -> IO b
promptInt s gte lte = do
                        putStr s
                        line <- getLine
                        if isJust(maybeRead line) &&
                           fromJust (maybeRead line) >= gte &&
                           fromJust (maybeRead line) <= lte
                          then return (fromJust $ maybeRead line)
                          else promptInt ("Invalid choice. Choose something between "
                                         ++ show gte ++ " and " ++ show lte ++ ".\n") gte lte
