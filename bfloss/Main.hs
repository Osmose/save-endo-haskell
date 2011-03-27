{-
 - Author:  Brianna Floss, bfloss@my.fit.edu
 - Course:  CSE 5400, Spring 2011
 - Project: endo1
 -}
 
module Main where
 
import Text.Printf

main :: IO()
main = interact (showResults . map solve . readTestCases)

type RNA = [Char]

showResults :: [RNA] -> RNA
showResults = unlines . (map printf)

readTestCases :: RNA -> [RNA]
readTestCases [] = []
readTestCases xs = (take 7 xs) : (readTestCases (drop 7 xs))

solve :: RNA -> RNA
solve xs = if(length xs) == 7
           then parse xs
		   else "SHORT"

parse :: RNA -> RNA
parse ('P':'I':'P':'I':'I':xs) = pipii xs
parse ('P':'I':'I':xs) = pii xs
parse ('P':'C':'C':xs) = pcc xs
parse ('P':'F':'F':xs) = pff xs
parse _ = "UNKNOWN"

pipii :: RNA -> RNA
pipii ('I':'C':_) = "black"
pipii ('I':'P':_) = "red"
pipii ('C':'C':_) = "green"
pipii ('C':'F':_) = "yellow"
pipii ('C':'P':_) = "blue"
pipii ('F':'C':_) = "magenta"
pipii ('F':'F':_) = "cyan"
pipii ('P':'C':_) = "white"
pipii ('P':'F':_) = "trans"
pipii ('P':'P':_) = "opaque"
pipii _ = "UNKNOWN"

pii :: RNA -> RNA
pii ('P':'I':'C':'P':_) = "empty"
pii ('I':'I':'I':'P':_) = "move"
pii ('P':'I':'I':'P':_) = "fill"
pii _ = "UNKNOWN"

pcc :: RNA -> RNA
pcc ('C':'C':'C':'P':_) = "ccw"
pcc ('I':'F':'F':'P':_) = "mark"
pcc ('P':'F':'F':'P':_) = "add"
pcc _ = "UNKNOWN"

pff :: RNA -> RNA
pff ('F':'F':'F':'P':_) = "cw"
pff ('I':'C':'C':'P':_) = "line"
pff ('P':'C':'C':'P':_) = "compose"
pff ('I':'C':'C':'F':_) = "clip"
pff _ = "UNKNOWN"