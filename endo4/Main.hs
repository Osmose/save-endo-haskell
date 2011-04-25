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
pattern (dna, rna) = parsePattern (dna,rna) 0 []


{-
Testcases:(Copy/paste into GHCI)
This is "I"
parsePattern (Data.Sequence.fromList "CIIC", Data.Sequence.empty) 0 []
[PBase "I"]

This is "(!2)P"
parsePattern (Data.Sequence.fromList "IIPIPICPIICICIIF", Data.Sequence.empty) 0 []
[Open,Skip 2,Close,PBase "P"]

I think this is the correct pattern for (?"ICFP")
parsePattern (Data.Sequence.fromList "IIPIFCCFPICIICIIF", Data.Sequence.empty) 0 []
[Open,Search "ICFP",Close]
-}
parsePattern :: GlobalState->Int->Pattern->(GlobalState,Pattern)
parsePattern (dna,rna) lvl p
    | d1 == 'C'  = parsePattern (DS.drop  1 dna,rna) lvl (p++(PBase "I"):[])
    | d1 == 'F'  = parsePattern (DS.drop  1 dna,rna) lvl (p++(PBase "C"):[])
    | d1 == 'P'  = parsePattern (DS.drop  1 dna,rna) lvl (p++(PBase "F"):[])
    | d2 == cIC  = parsePattern (DS.drop  2 dna,rna) lvl (p++(PBase "P"):[])
    | d2 == cIP  = patternNat   (DS.drop  2 dna,rna) lvl p
    | d2 == cIF  = patternConst (DS.drop  3 dna,rna) lvl p
    | d3 == cIIP = parsePattern (DS.drop  3 dna,rna) (lvl+1) (p++Open:[])
    | d3 == cIII = parsePattern (DS.drop 10 dna,rna><rnaAdd) lvl p
    | d3 == cIIC || d3 == cIIF =
        if lvl==0 then ((DS.drop 3 dna, rna),p)
        else      parsePattern (DS.drop  3 dna,rna) (lvl-1) (p++Close:[])
    | otherwise = ((dna,rna),p) -- Need to indicate finish here...
      where
        d1 = DS.index dna 0
        d2 = DS.take 2 dna
        d3 = DS.take 3 dna

        -- For the command III, gets the 7 characters that would be inserted
        rnaAdd = (DS.take 7 (DS.drop 3 dna))

        -- Silly constants
        cIC  = DS.fromList "IC"
        cIP  = DS.fromList "IP"
        cIF  = DS.fromList "IF"
        cIIP = DS.fromList "IIP"
        cIIC = DS.fromList "IIC"
        cIIF = DS.fromList "IIF"
        cIII = DS.fromList "III"

patternNat :: GlobalState->Int->Pattern->(GlobalState,Pattern)
patternNat (dna,rna) lvl p = parsePattern (ndna,rna) lvl (p++(Skip n):[])
    where
        (ndna,n) = nat dna

-- If this isn't correct, then it should probably be "Search (reverse str)"
patternConst :: GlobalState->Int->Pattern->(GlobalState,Pattern)
patternConst (dna,rna) lvl p = parsePattern (ndna,rna) lvl (p++(Search str):[])
    where
        (ndna,str) = consts dna

-- Templates
type Template = [TItem]
data TItem = TBase String | Ref Int Int | RefLen Int deriving (Eq, Show)

-- Decodes a template
template :: GlobalState -> (GlobalState, Template)
template (dna, rna) = parseTemplate (dna,rna) []

{-
Testcases:(Copy/paste into GHCI)
parseTemplate (Data.Sequence.fromList "CFPICIFCCPCFPIIPCCPIICII", Data.Sequence.empty) []
((fromList "II",fromList ""),[TBase "I",TBase "C",TBase "F",TBase "P",Ref 1 3,RefLen 3])

This code was essentially copied from parsePattern, so if you fix something here,
fix it there as well.
-}
parseTemplate :: GlobalState->Template->(GlobalState,Template)
parseTemplate (dna,rna) t
    | d1 == 'C'  = parseTemplate (DS.drop  1 dna,rna) (t++(TBase "I"):[])
    | d1 == 'F'  = parseTemplate (DS.drop  1 dna,rna) (t++(TBase "C"):[])
    | d1 == 'P'  = parseTemplate (DS.drop  1 dna,rna) (t++(TBase "F"):[])
    | d2 == cIC  = parseTemplate (DS.drop  2 dna,rna) (t++(TBase "P"):[])
    | d2 == cIP  || d2 == cIF = let
                                  (tdna, l) = nat (DS.drop 2 dna)
                                  (ndna, n) = nat tdna
                                in parseTemplate (ndna,rna) (t++(Ref n l):[])

    | d3 == cIIP = let (ndna,n) = nat (DS.drop 3 dna)
                   in parseTemplate (ndna, rna) (t++(RefLen n):[])
    | d3 == cIII = parseTemplate (DS.drop 10 dna,rna><rnaAdd) t
    | d3 == cIIC || d3 == cIIF =((DS.drop 3 dna, rna),t)
    | otherwise = ((dna,rna),t) -- Need to indicate finish here...
      where
        d1 = DS.index dna 0
        d2 = DS.take 2 dna
        d3 = DS.take 3 dna

        -- For the command III, gets the 7 characters that would be inserted
        rnaAdd = (DS.take 7 (DS.drop 3 dna))

        -- Silly constants
        cIC  = DS.fromList "IC"
        cIP  = DS.fromList "IP"
        cIF  = DS.fromList "IF"
        cIIP = DS.fromList "IIP"
        cIIC = DS.fromList "IIC"
        cIIF = DS.fromList "IIF"
        cIII = DS.fromList "III"



-- Pattern Matching
matchreplace :: GlobalState -> Pattern -> Template -> GlobalState
matchreplace state pat temp = state



-- Helper functions

{- Testcases:(Copy/Paste into GHCI)
asNat 5
fromList "CICP"
nat (asNat 134)
(fromList "",134)
-}
asNat :: Int->Seq Char
asNat n
    | n == 0        = DS.fromList "P"
    | n>0 && even n = 'I' <| asNat (n `div` 2)
    | n>0 && odd n  = 'C' <| asNat (n `div` 2)

{- Testcases:(Copy/Paste into GHCI)
protect 1 (Data.Sequence.fromList "ICFP")
fromList "CFPIC"
protect 2 (Data.Sequence.fromList "ICFP")
fromList "FPICCF"
-}
-- Takes in a count, string to protect, and global state
protect :: Int->Seq Char->Seq Char
protect 0 d = d
protect l d = protect (l-1) (quote d)

{- Testcases:(Copy/Paste into GHCI)
quote (Data.Sequence.fromList "ICFP")
fromList "CFPIC"
-}
-- Encodes a string
quote :: Seq Char->Seq Char
quote d
    | DS.null d = DS.empty
    | d1 == 'I' = 'C' <| quote (DS.drop 1 d)
    | d1 == 'C' = 'F' <| quote (DS.drop 1 d)
    | d1 == 'F' = 'P' <| quote (DS.drop 1 d)
    | d1 == 'P' = 'I' <| 'C' <| quote (DS.drop 1 d)
    | otherwise = DS.empty
      where
        d1 = DS.index d 0

-- Consumes some DNA producing a number
-- Input:  DNA
-- Output: (remaining dna, number)
nat :: Seq Char -> (Seq Char,Int)
nat s | DS.null s              = (DS.empty, 0) -- The finish clause...
      | d1 == 'P'              = (DS.drop 1 s,0)
      | d1 == 'I' || d1 == 'F' = (dna,2 * n)
      | d1 == 'C'              = (dna,2 * n + 1)
      | otherwise              = (DS.drop 1 s,0)
        where
          d1 = DS.index s 0
          (dna,n) = nat (DS.drop 1 s)
-- Consumes some DNA to producing a search string
-- Input:  DNA
-- Output: (remaining dna,search string)
-- Yes, I'm aware this is ugly as sin...
consts :: Seq Char -> (Seq Char, String)
consts s | d1 == 'C'  = let
                            (rdna, str) = consts (DS.drop 1 s)
                            str2 = 'I':str
                        in (rdna, str2)
         | d1 == 'F'  = let
                            (rdna, str) = consts (DS.drop 1 s)
                            str2 = 'C':str
                        in (rdna, str2)
         | d1 == 'P'  = let
                            (rdna, str) = consts (DS.drop 1 s)
                            str2 = 'F':str
                        in (rdna, str2)
         | d2  == cIC = let
                            (rdna, str) = consts (DS.drop 2 s)
                            str2 = 'P':str
                        in (rdna, str2)
         | otherwise  = (s,[])
           where
             d1 = DS.index s 0
             d2 = DS.take 2 s
             cIC = DS.fromList "IC"