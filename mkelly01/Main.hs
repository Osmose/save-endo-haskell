{-
 - Author: Michael Kelly, mkelly01@my.fit.edu
 - Course: CSE 4510, Spring 2011
 - Project: endo1, Saving Endo, Part I
 -}

module Main where

import Data.List

-- Main function
main :: IO()
main = interact (unlines . parseGCLSequence initState . lines)

type ColorBucket = ((Int, Int, Int), Int)
type AlphaBucket = (Int, Int)

data Bucket = Bucket ColorBucket AlphaBucket deriving (Show)

-- One of the cardinal directions
data Facing = North | East | South | West deriving (Enum, Ord, Eq, Show)

-- Contains the current state of the bucket and movement commands
-- The two pairs are, in order, the current mark and current position
data State = State Bucket Facing (Int, Int) (Int, Int) deriving (Show)

-- Initial state
emptyBucket = (Bucket ((0, 0, 0), 0) (0, 0))
initState = (State emptyBucket East (0,0) (0,0))

-- Colors
black   = (0,   0,   0  )
red     = (255, 0,   0  )
green   = (0,   255, 0  )
yellow  = (255, 255, 0  )
blue    = (0,   0,   255)
magenta = (255, 0,   255)
cyan    = (0,   255, 255)
white   = (255, 255, 255)

-- Alpha
trans = 0
opaque = 255

-- Steps through the list of GCL commands and parses them, maintaining state
-- using the State type
parseGCLSequence :: State -> [String] -> [String]
parseGCLSequence _ [] = []
parseGCLSequence state (x:xs)
    | null nx = parseGCLSequence nstate xs
    | otherwise = nx:(parseGCLSequence nstate xs)
    where (nx,nstate) = parseGCLCommand x state
    
-- Transforms the state and generates the new command from a GCL command
parseGCLCommand :: String -> State -> (String, State)

-- Bucket commands
parseGCLCommand "black"   state = ("", addColor black state)
parseGCLCommand "red"     state = ("", addColor red state)
parseGCLCommand "green"   state = ("", addColor green state)
parseGCLCommand "yellow"  state = ("", addColor yellow state)
parseGCLCommand "blue"    state = ("", addColor blue state)
parseGCLCommand "magenta" state = ("", addColor magenta state)
parseGCLCommand "cyan"    state = ("", addColor cyan state)
parseGCLCommand "white"   state = ("", addColor white state)
parseGCLCommand "trans"  state = ("", addAlpha trans state)
parseGCLCommand "opaque" state = ("", addAlpha opaque state)
parseGCLCommand "empty" (State _ face mark pos) = ("", (State emptyBucket face mark pos))

-- Movement
parseGCLCommand "move" state = ("", moveState state)
parseGCLCommand "ccw" (State bucket face mark pos)
    | face == North = res West
    | otherwise = res (pred face)
    where res newFace = ("", (State bucket newFace mark pos))
parseGCLCommand "cw" (State bucket face mark pos)
    | face == West = res North
    | otherwise = res (succ face)
    where res newFace = ("", (State bucket newFace mark pos))
parseGCLCommand "mark" (State bucket face _ pos) = ("", (State bucket face pos pos))

-- Drawing
parseGCLCommand "line" state@(State bucket face mark pos) = ("line from " ++ show pos ++ " to " ++ show mark ++ "; " ++ bucketToString bucket, state)
parseGCLCommand "fill" state@(State bucket face mark pos) = ("fill at" ++ show pos ++ "; " ++ bucketToString bucket, state)

-- Fallback
parseGCLCommand cmd state = (cmd, state)


-- Adds a color to the bucket in a state
addColor :: (Int, Int, Int) -> State -> State
addColor (nr, ng, nb) (State (Bucket ((r, g, b), count) alpha) face mark pos) = (State (Bucket ((r + nr, g + ng, b + nb), count + 1) alpha) face mark pos)
    
-- Adds an alpha value to the bucket in a state
addAlpha :: Int -> State -> State
addAlpha addAlpha (State (Bucket colors (alpha, count)) face mark pos) = (State (Bucket colors (alpha + addAlpha, count + 1)) face mark pos)

-- Alters a state to a new position based on it's facing
moveState :: State -> State
moveState (State bucket face mark pos) = (State bucket face mark (moveCoords face pos))
    
-- Alters coordinates based on the given facing
moveCoords :: Facing -> (Int, Int) -> (Int, Int)
moveCoords face (x, y)
    | face == North = (x, (y + 599) `mod` 600)
    | face == South = (x, (y + 1) `mod` 600)
    | face == East = ((x + 1) `mod` 600, y)
    | face == West = ((x + 599) `mod` 600, y)
    
bucketToString :: Bucket -> String
bucketToString (Bucket ((r, g, b), cc) (alpha, ac)) = "color (" ++ concat (intersperse "," ([showColor r cc, showColor g cc, showColor b cc, showAlpha alpha ac])) ++ ")"

showAvg :: Int -> Int -> Int -> String
showAvg def val count
    | count == 0 = show def
    | otherwise = show (val `div` count)
    
showColor :: Int -> Int -> String
showColor = showAvg 0

showAlpha :: Int -> Int -> String
showAlpha = showAvg 255