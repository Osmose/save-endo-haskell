{-
 - Course: CSE 4510, Spring 2011
 - Project: endo4, Saving Endo
 -}

module Main where
import Data.Sequence as DS
import Data.Foldable
import Data.Either

--import Debug.Trace


trace :: String->a->a
trace _ a = a

-- Main function
main :: IO()
main = interact( unlines . map parseInput . lines)

-- (DNA, RNA, finished?)
type GlobalState = (Seq Char, Seq Char, Bool)

-- Passes the dna string to execute and prints out the resulting rna
parseInput :: String -> String
parseInput dna = toList $ executeOnce (DS.fromList dna, DS.empty, False)

-- Executes a DNA string to produce an RNA string
{-
 - execute :: GlobalState -> Seq Char
execute state = either id execute result
    where
        result = executeOnce state
-}
-- Runs through the execute loop once
executeOnce :: GlobalState ->Seq Char
executeOnce state@(dna, rna, _)
    | f1 == True || f2 == True = rna
    -- | pat == [] = Left rna
    -- | temp== [] = trace ("--" ++ prettyPattern pat) Left rna
    | otherwise = trace ((prettyPattern pat) ++ "\n" ++ ((prettyTemplate temp)) ++ "\nlen(rna)   "++show (DS.length nrna `div` 7)++ "\nlen(ndna)  "++show (DS.length ndna)++ "\nlen(dna)   "++show (DS.length dna)++"\n") (executeOnce newState)
    where
        (pstate@(_,_,f1), pat) = pattern state
        (tstate@(_,_,f2), temp) = template pstate
        newState@(ndna,nrna,_) = matchreplace tstate pat temp

prettyTemplate :: Template->String
prettyTemplate [] = []
prettyTemplate ((TBase x):xs)  = x:prettyTemplate xs
prettyTemplate ((Ref n 0):xs)  = "\\" ++ show n ++ prettyTemplate xs
prettyTemplate ((Ref n l):xs)  = "\\(" ++ show n ++ "," ++ show l ++")" ++ prettyTemplate xs
prettyTemplate ((RefLen l):xs) = "(rl:"++show l++")" ++ prettyTemplate xs

prettyPattern :: Pattern->String
prettyPattern [] = []
prettyPattern ((PBase x):xs)  = x:prettyPattern xs
prettyPattern ((Skip n):xs)  = "!" ++ show n ++ prettyPattern xs
prettyPattern ((Search s):xs)  = "?" ++ show s ++"" ++ prettyPattern xs
prettyPattern ((Open):xs) = "(" ++ prettyPattern xs
prettyPattern ((Close):xs) = ")" ++ prettyPattern xs


-- Patterns
type Pattern = [PItem]
data PItem = PBase Char | Skip Int | Search String | Open | Close deriving (Eq, Show)

-- Decodes a pattern
pattern :: GlobalState -> (GlobalState, Pattern)
pattern state = parsePattern state 0 []


{-
Testcases:(Copy/paste into GHCI)
This is "I"
parsePattern (Data.Sequence.fromList "CIIC", Data.Sequence.empty) 0 []
[PBase 'I']

This is "(!2)P"
parsePattern (Data.Sequence.fromList "IIPIPICPIICICIIF", Data.Sequence.empty) 0 []
[Open,Skip 2,Close,PBase 'P']

I think this is the correct pattern for (?"ICFP")
parsePattern (Data.Sequence.fromList "IIPIFCCFPICIICIIF", Data.Sequence.empty) 0 []
[Open,Search "ICFP",Close]
-}
parsePattern :: GlobalState->Int->Pattern->(GlobalState,Pattern)
parsePattern (dna,rna,f) lvl p
    | f == True   = ((dna,rna,True),[])
    | DS.null dna = ((dna,rna,True),[])
    | d1 == 'C'  = parsePattern (DS.drop  1 dna,rna, f) lvl (p++(PBase 'I'):[])
    | d1 == 'F'  = parsePattern (DS.drop  1 dna,rna, f) lvl (p++(PBase 'C'):[])
    | d1 == 'P'  = parsePattern (DS.drop  1 dna,rna, f) lvl (p++(PBase 'F'):[])
    | d2 == cIC  = parsePattern (DS.drop  2 dna,rna, f) lvl (p++(PBase 'P'):[])
    | d2 == cIP  = patternNat   (DS.drop  2 dna,rna, f) lvl p
    | d2 == cIF  = patternConst (DS.drop  3 dna,rna, f) lvl p
    | d3 == cIIP = parsePattern (DS.drop  3 dna,rna, f) (lvl+1) (p++Open:[])
    | d3 == cIII = parsePattern (DS.drop 10 dna,rna><rnaAdd, f) lvl p
    | d3 == cIIC || d3 == cIIF =
        if lvl==0 then ((DS.drop 3 dna, rna, f),p)
        else      parsePattern (DS.drop  3 dna,rna, f) (lvl-1) (p++Close:[])
    | otherwise = ((dna,rna,True),[])
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
patternNat (dna,rna,_) lvl p = parsePattern (ndna,rna,finished) lvl (p++(Skip n):[])
    where
        (ndna,n,finished) = nat dna

-- If this isn't correct, then it should probably be "Search (reverse str)"
patternConst :: GlobalState->Int->Pattern->(GlobalState,Pattern)
patternConst (dna,rna,f) lvl p = parsePattern (ndna,rna,f) lvl (p++(Search str):[])
    where
        (ndna,str) = consts dna

-- Templates
type Template = [TItem]
data TItem = TBase Char | Ref Int Int | RefLen Int deriving (Eq, Show)

-- Decodes a template
template :: GlobalState -> (GlobalState, Template)
template (dna, rna, f) = parseTemplate (dna,rna, f) []

{-
Testcases:(Copy/paste into GHCI)
parseTemplate (Data.Sequence.fromList "CFPICIFCCPCFPIIPCCPIICII", Data.Sequence.empty) []
((fromList "II",fromList ""),[TBase 'I',TBase 'C',TBase 'F',TBase 'P',Ref 1 3,RefLen 3])

This code was essentially copied from parsePattern, so if you fix something here,
fix it there as well.
-}
parseTemplate :: GlobalState->Template->(GlobalState,Template)
parseTemplate (dna,rna,f) t
    | DS.null dna = ((dna,rna, True),[])
    | d1 == 'C'  = parseTemplate (DS.drop  1 dna,rna,f) (t++(TBase 'I'):[])
    | d1 == 'F'  = parseTemplate (DS.drop  1 dna,rna,f) (t++(TBase 'C'):[])
    | d1 == 'P'  = parseTemplate (DS.drop  1 dna,rna,f) (t++(TBase 'F'):[])
    | d2 == cIC  = parseTemplate (DS.drop  2 dna,rna,f) (t++(TBase 'P'):[])
    | d2 == cIP  || d2 == cIF = let
                                  (tdna, l, f1) = nat (DS.drop 2 dna)
                                  (ndna, n, f2) = nat tdna
                                in
                                  if (f1 == True || f2 == True) then ((dna,rna,True),[])
                                  else parseTemplate (ndna,rna,f) (t++(Ref n l):[])

    | d3 == cIIP = let (ndna,n,f) = nat (DS.drop 3 dna)
                   in
                     if (f==True) then ((dna,rna,True),[])
                     else parseTemplate (ndna, rna, f) (t++(RefLen n):[])
    | d3 == cIII = parseTemplate (DS.drop 10 dna,rna><rnaAdd, f) t
    | d3 == cIIC || d3 == cIIF =((DS.drop 3 dna, rna, f),t)
    | otherwise = ((dna,rna,True),[])
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


prettyEnv :: Environment -> String
prettyEnv [] = []
prettyEnv (x:xs) = "\n   "++(toList (DS.take 10 x)) ++"... (" ++ show (DS.length x) ++ " bases)" ++ prettyEnv xs


-- Pattern Matching
matchreplace :: GlobalState -> Pattern -> Template -> GlobalState
matchreplace gs pat temp = mr (0,[],[]) pat temp gs

-- current position, Environment, Marked open parens
type MRState = (Int,Environment, [Int])
mr :: MRState->Pattern->Template->GlobalState->GlobalState
mr (i,e,c) []      t gs@(dna,rna,f) = trace ("  successful match of length " ++ show i ++ " " ++prettyEnv e) replace t e ((DS.drop i dna),rna,f) DS.empty
mr (i,e,c) (p:pat) t gs@(dna,rna,f) = handlePat p
    where
        handlePat (PBase b) | i>=DS.length dna = gs
                            | otherwise = if (DS.index dna i == b) then mr(i+1,e,c) pat t gs
                                          else gs
        handlePat (Skip n)  | i>=DS.length dna = gs
                            | otherwise = if (i+n > DS.length dna) then gs
                                          else mr (i+n,e,c) pat t gs
        handlePat (Search s)  | i>=DS.length dna = gs
                              | otherwise = let x = i + findPos s (DS.drop i dna) in
                                              if x<i then gs
                                              else mr (x,e,c) pat t gs
        handlePat (Open)     = mr (i,e,i:c) pat t gs
        handlePat (Close)    = let
                                 (cc:nc) = c
                                 ne = e++[DS.take (i-cc) (DS.drop cc dna)]
                               in mr (i,ne,nc) pat t gs

-- Searches for string in sequence, returning the position of it if found
findPos :: String->Seq Char->Int
findPos needle haystack  = findHelper 0 needle haystack

findHelper :: Int->String->Seq Char->Int
findHelper i s dna
    | DS.null dna = (-1)
    | DS.fromList s == DS.take (Prelude.length s) dna = i+(Prelude.length s)
    | otherwise = findHelper (i+1) s (DS.drop 1 dna)


-- data PItem = PBase Char | Skip Int | Search String | Open | Close deriving (Eq, Show)

-- Helper functions

{-
-}
type Environment = [Seq Char]

{- Testcases:(Copy/Paste into GHCI)
replace [TBase 'P',RefLen 0,Ref 0 1] [Data.Sequence.fromList "ICFP"] (Data.Sequence.empty,Data.Sequence.empty) Data.Sequence.empty
(fromList "PIICPCFPIC",fromList "")
-}
-- Takes a template, environment, current string being built, globalstate, and transforms the dna
replace :: Template->Environment->GlobalState->Seq Char->GlobalState
replace []     e (dna,rna,f) r = (r >< dna, rna,f)
replace (t:ts) e (dna,rna,f) r = replace ts e (dna,rna,f) (replaceItem t e r)

-- Replaces a single template item, adding it to the replacement string
replaceItem :: TItem->Environment->Seq Char->Seq Char
replaceItem (TBase t)  e r = r |> t
replaceItem (Ref n l)  e r = r >< protect l env
  where env = if (n >= Prelude.length e) then DS.empty else e!!n
replaceItem (RefLen n) e r = r >< asNat len
  where len = if (n>= Prelude.length e) then 0 else DS.length (e!!n)

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
-- Output: (remaining dna, number, finished?)
nat :: Seq Char -> (Seq Char,Int,Bool)
nat s | DS.null s              = (DS.empty, 0,True)
      | d1 == 'P'              = (DS.drop 1 s,0,False)
      | d1 == 'I' || d1 == 'F' = (dna,2 * n,False)
      | d1 == 'C'              = (dna,2 * n + 1,False)
      | otherwise              = (DS.drop 1 s,0,False)
        where
          d1 = DS.index s 0
          (dna,n,f) = nat (DS.drop 1 s)
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
