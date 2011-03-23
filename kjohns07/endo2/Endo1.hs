{-
 - Author: Keith Johnson, kjohns07@my.fit.edu
 - Course: CSE5400, Spring 2011
 - Project: endo1
-}

module Main where

import Text.Printf
main :: IO()
main = Prelude.interact(parseInput)

parseInput :: String->String
parseInput "" = ""
parseInput x = '\n':(trans (take 7 x))++ parseInput (drop 7 x)

trans :: String->String
trans "PIPIIIC" = "black"
trans "PIPIIIP" = "red"
trans "PIPIICC" = "green"
trans "PIPIICF" = "yellow"
trans "PIPIICP" = "blue"
trans "PIPIIFC" = "magenta"
trans "PIPIIFF" = "cyan"
trans "PIPIIPC" = "white"
trans "PIPIIPF" = "trans"
trans "PIPIIPP" = "opaque"

trans "PIIPICP" = "empty"
trans "PIIIIIP" = "move"
trans "PCCCCCP" = "ccw"
trans "PFFFFFP" = "cw"
trans "PCCIFFP" = "mark"
trans "PFFICCP" = "line"
trans "PIIPIIP" = "fill"
trans "PCCPFFP" = "add" -- creates new bitmap
trans "PFFPCCP" = "compose"
trans "PFFICCF" = "clip"
trans x
    | length x /= 7 = "SHORT"
    | otherwise     = "UNKNOWN"
