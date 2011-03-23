{-
 - Author: Michael Kelly, mkelly01@my.fit.edu
 - Course: CSE 4510, Spring 2011
 - Project: endo1, Saving Endo, Part I
 -}

module Main where

-- Main function
main :: IO()
main = interact (unlines . parseGCLSequence . lines)

-- Initial state
emptyBucket = (Bucket 0 0 0 0 0 0)
initState = (State emptyBucket East (0,0) (0,0))

-- A Bucket is four numbers for the red, green, blue, and alpha values,
-- a fifth number for the amount of colors, and a sixth for the amount of alphas
type Bucket = Bucket Int Int Int Int Int Int

-- One of the cardinal directions
type Facing = North | South | East | West

-- Contains the current state of the bucket and movement commands
-- The two pairs are, in order, the current mark and current position
type State = State Bucket Facing (Int, Int) (Int, Int)

-- Colors
black   = (0,   0,   0  )
red     = (255, 0,   0  )
green   = (0,   255, 0  )
yellow  = (255, 255, 0  )
blue    = (0,   0,   255)
magenta = (255, 0,   255)
cyan    = (0,   255, 255)
white   = (255, 255, 255)

-- Steps through the list of GCL commands and parses them, maintaining state
-- using the State type
parseGCLSequence :: [String] -> State ->  [String]
parseGCLSequence [] _ = []
parseGCLSequence x:xs state
    | nx == "" = parseGCLSequence xs nstate
    | otherwise = nx:(parseGCLSequence xs nstate)
    where (nx,nstate) = parseGCLCommand nx nstate
    
-- Transforms the state and generates the new command from a GCL command
parseGCLCommand :: String -> State -> (String, State)

-- Color commands
parseGCLCommand "black"   state = ("", addColor(black, state))
parseGCLCommand "red"     state = ("", addColor(red, state))
parseGCLCommand "green"   state = ("", addColor(green, state))
parseGCLCommand "yellow"  state = ("", addColor(yellow, state))
parseGCLCommand "blue"    state = ("", addColor(blue, state))
parseGCLCommand "magenta" state = ("", addColor(magenta, state))
parseGCLCommand "cyan"    state = ("", addColor(cyan, state))
parseGCLCommand "white"   state = ("", addColor(white, state))

-- Alpha
parseGCLCommand "trans"  state = ("", addAlpha(0, state))
parseGCLCommand "opaque" state = ("", addAlpha(255, state))

parseGCLCommand "empty" (State _ face mark pos) = ("", (State emptyBucket face mark pos))

parseGCLCommand "move" state =
parseGCLCommand "ccw" state =
parseGCLCommand "cw" state =
parseGCLCommand "mark" state =
parseGCLCommand "line" state =
parseGCLCommand "fill" state =
parseGCLCommand "add" state =
parseGCLCommand "compose" state =
parseGCLCommand "clip" state =

-- Adds a color to the bucket in a state
addColor :: (Int, Int, Int) -> State -> State
addColor (nr, ng, nb) (State (Bucket r g b a cc ac) face mark pos) = 
    (State (Bucket r + nr, g + ng, b + nb, a, cc + 1, ac) face mark pos)
    
-- Adds an alpha value to the bucket in a state
addAlpha :: Int -> State -> State
addAlpha na (State (Bucket r g b a cc ac) face mark pos) = 
    (State (Bucket r, g, b, a + na, cc, ac + 1) face mark pos)