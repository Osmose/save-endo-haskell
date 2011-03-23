
{-
 - Author: Keith Johnson, kjohns07@my.fit.edu
 - Course: CSE5400, Spring 2011
 - Project: endo2
-}

module Main where

main :: IO()
main = interact(unlines . flip parseCommands initialState . lines)

---------------------------------
--  Type/Constant definitions  --
---------------------------------
type Coord = Int
type Pos = (Coord, Coord)
type Component = Int
type Color = (Component, Component, Component)
type Transparency = Int
type Pixel = (Color,Transparency)
type Bitmap = [[Pixel]]
type Bitmaps = [Bitmap]
type Dir = Char		-- Direction Type(N,S,E,W)

type State = (Pos, Dir, Pos, (Color,Int), (Transparency,Int), Bitmaps)
{- In order, state contains
    * Current position
    * Current direction(NSWE)
    * Marked position
    * Current bucket color, # of colors in bucket
    * Current bucket transparency, # of transparency commands
    * Bitmaps(Currently unused)
-}

-- Define the basic colors
black   = (  0,   0,   0)
red     = (255,   0,   0)
green   = (  0, 255,   0)
yellow  = (255, 255,   0)
blue    = (  0,   0, 255)
magenta = (255,   0, 255)
cyan    = (  0, 255, 255)
white   = (255, 255, 255)

transparent = 0
opaque      = 255

-- Define the initial state of the program(location (0,0) facing East)
initialState = ((0,0), 'E', (0,0), ((0,0,0),0), (opaque,0), [[]])


---------------------------------
--    Function definitions     --
---------------------------------

{- Provided a list of commands, and an initial state, it will execute a command,
 - which may do the following:
    * Modify the state
    * Change the string printed(i.e. "fill" to "fill at (x,y); color (r,g,b,a)")
    * Do nothing(ignore the command)
-}
parseCommands :: [String]->State->[String]
parseCommands [] s = []
parseCommands (x:xs) s = if null a
			    then parseCommands xs b
			    else a:parseCommands xs b
    where (a,b) = command (x,s)

{- Performs the actual transformation from a commmand and a state, to a new
 - output string, and a new state
 -}
command:: (String,State)->(String,State)
-- Handle the movement commands(affect the first 3 elements of State)
command ("move",s) = ("", move s)
command ("mark",s) = ("", mark s)
command ("cw",s)   = ("", cw s)
command ("ccw",s)  = ("", ccw s)

-- Handle the various color commands
command ("black", s)   = ("", addColor s black)
command ("red", s)     = ("", addColor s red)
command ("green", s)   = ("", addColor s green)
command ("yellow", s)  = ("", addColor s yellow)
command ("blue", s)    = ("", addColor s blue)
command ("magenta", s) = ("", addColor s magenta)
command ("cyan", s)    = ("", addColor s cyan)
command ("white", s)   = ("", addColor s white)

-- Handle alpha channel commands
command ("trans", s) = ("", addTrans s transparent)
command ("opaque", s)      = ("", addTrans s opaque)

-- The two commands we support transforming
command ("line", s)  = (makeLine s, s)
command ("fill", s)  = (makeFill s, s)

-- A special command to reset the bucket
command ("empty", s) = ("", resetBucket s)

-- Pass through anything we don't recognize
command (x,s) = (x,s)


-- Reset the state of a bucket(Clears colors/transparency)
resetBucket :: State -> State
resetBucket (p,d,m,_,_,b) = (p,d,m,((0,0,0),0),(opaque,0),b)

-- Rotates a direction clockwise or counterclockwise
cwDir :: Dir -> Dir
cwDir d | d == 'N' = 'E'
	| d == 'W' = 'N'
	| d == 'S' = 'W'
	| d == 'E' = 'S'
ccwDir :: Dir -> Dir
ccwDir d | d == 'N' = 'W'
	 | d == 'W' = 'S'
	 | d == 'S' = 'E'
	 | d == 'E' = 'N'

-- Rotates the direction in a state clockwise or counterclockwise
cw :: State -> State
cw (a,b,c,d,e,f) = (a,cwDir b,c,d,e,f)
ccw :: State -> State
ccw (a,b,c,d,e,f) = (a,ccwDir b,c,d,e,f)

-- The mark command(Copies current position into the marked position)
mark :: State -> State
mark (p, d, m, c, t, b) = (p, d, p, c, t, b)

-- Move(Updates the position based on direction, bounded to a 600x600 area)
move :: State -> State
move (p, d, m, c, t, b) = (moveDir p d, d, m, c, t, b)
moveDir :: Pos->Dir->Pos
moveDir (x,y) d | d=='N' = (x, (y + 599) `mod` 600)
		| d=='S' = (x, (y +   1) `mod` 600)
		| d=='W' = ((x + 599) `mod` 600, y)
		| d=='E' = ((x +   1) `mod` 600, y)

{- Adds a color to the current bucket color
 - In state, we store the current color(an average of all the colors), and the
 - number of colors we have averaged so far.  To add a new color, we multiply
 - each color value by the number we have averaged, add the new color channel,
 - then divide by the new number we are averaging by.

 - Basically this keeps a rolling/current average of the contents of the bucket,
 - without storing each value in the bucket.
 -}
addColor :: State->Color-> State
addColor (p, d, m, c, t, b) color = (p, d, m, (updateColor c color), t, b)

{- Transparency works the same as the color method above, but it only operates
 - on the single alpha channel
 -}
addTrans :: State->Transparency->State
addTrans (p, d, m, c, t, b) trans = (p, d, m, c, updateTrans t trans, b)

-- These two functions do the actual rolling average(see addColors description)
updateColor:: (Color,Int)->Color->(Color,Int)
updateColor (x,0) c = (c,1)
updateColor ((r,g,b),l) (ar,ag,ab) = ((nr, ng, nb),l+1)
    where
	  nr = (r*l+ar) `div` l
	  ng = (g*l+ag) `div` l
	  nb = (b*l+ab) `div` l

updateTrans :: (Transparency,Int)->Transparency->(Transparency,Int)
updateTrans (_,0) t  = (t,1)
updateTrans (t,l) nt = ((t*l + nt) `div` l, l+1)

-- Formats a Color/Transparency as "(r,g,b,a)" for printing
strColor :: Color->Transparency->String
strColor (r,g,b) a = "color " ++ show (r,g,b,a)

{- Uses the current known state, generate a proper "line" command with the
 - correct coordinates and color value
 -}
makeLine :: State->String
makeLine (p1,_,p2,(c,_),(t,_),_) = "line from" ++ (show p1) ++ " to " ++ (show p2) ++ "; " ++ (strColor c t)

-- Same basic idea of makeLine; uses the current state to print a fill command
makeFill :: State->String
makeFill (p,_,_,(c,_),(t,_),_) = "fill at " ++ (show p) ++ "; " ++ (strColor c t)