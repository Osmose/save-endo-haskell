{-
 - Course: CSE 4510, Spring 2011
 - Project: endo4, Saving Endo
 -}

module Main where
import Data.Sequence as DS
import Data.Foldable
import Data.Either

-- Main function
main :: IO()
main = interact parseInput

-- (DNA, RNA)
type GlobalState = (Seq Char, Seq Char)

-- Passes the dna string to execute and prints out the resulting rna
parseInput :: String -> String
parseInput dna = toList $ execute (DS.fromList dna, DS.empty)

-- Executes a DNA string to produce an RNA string
execute :: GlobalState -> Seq Char
execute state = either id execute result
    where
        result = executeOnce state

-- Runs through the execute loop once
executeOnce :: GlobalState -> Either (Seq Char) GlobalState
executeOnce state@(dna, rna)
    | pat == [] = Left rna
    | temp == [] = Left rna
    | otherwise = Right newState
    where
        (pstate, pat) = pattern state
        (tstate, temp) = template pstate
        newState = matchreplace tstate pat temp


-- Patterns
type Pattern = [PItem]
data PItem = PBase String | Skip Int | Search String | Open | Close deriving (Eq, Show)

-- Decodes a pattern
pattern :: GlobalState -> (GlobalState, Pattern)
pattern (dna, rna) = ((dna, rna), [])


-- Templates
type Template = [TItem]
data TItem = TBase String | Ref Int Int | RefLen Int deriving (Eq, Show)

-- Decodes a template
template :: GlobalState -> (GlobalState, Template)
template (dna, rna) = ((dna, rna), [])


-- Pattern Matching
matchreplace :: GlobalState -> Pattern -> Template -> GlobalState
matchreplace state pat temp = state
