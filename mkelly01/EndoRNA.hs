{-
 - Author: Michael Kelly, mkelly01@my.fit.edu
 - Course: CSE 4510, Spring 2011
 - Project: endo1, Saving Endo, Part I
 -}

module Main where

-- Main function
main :: IO()
main = interact (unlines . parseRNASequence)
    
parseRNASequence :: String -> [String]
parseRNASequence [] = []
parseRNASequence xs = (parseRNA (take 7 xs)) : (parseRNASequence (drop 7 xs))
    
parseRNA :: String -> String
parseRNA "PIPIIIC" = "black"
parseRNA "PIPIIIP" = "red"
parseRNA "PIPIICC" = "green"
parseRNA "PIPIICF" = "yellow"
parseRNA "PIPIICP" = "blue"
parseRNA "PIPIIFC" = "magenta"
parseRNA "PIPIIFF" = "cyan"
parseRNA "PIPIIPC" = "white"
parseRNA "PIPIIPF" = "trans"
parseRNA "PIPIIPP" = "opaque"
parseRNA "PIIPICP" = "empty"
parseRNA "PIIIIIP" = "move"
parseRNA "PCCCCCP" = "ccw"
parseRNA "PFFFFFP" = "cw"
parseRNA "PCCIFFP" = "mark"
parseRNA "PFFICCP" = "line"
parseRNA "PIIPIIP" = "fill"
parseRNA "PCCPFFP" = "add"
parseRNA "PFFPCCP" = "compose"
parseRNA "PFFICCF" = "clip"
parseRNA xs
    | length xs < 7 = "SHORT"
    | otherwise = "UNKNOWN"
