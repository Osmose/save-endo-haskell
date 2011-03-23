
{-
 - Author: Keith Johnson, kjohns07@my.fit.edu
 - Course: CSE5400, Spring 2011
 - Project: endo2
-}

module Main where

import Text.Printf
import Data.List

main :: IO()
main = interact(unlines . flip parseCommands initialState . lines)



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

initialState = ((0,0), 'E', (0,0), ((0,0,0),0), (opaque,0), [[]])


parseCommands :: [String]->State->[String]
parseCommands [] s = []
parseCommands (x:xs) s = if null a
			    then parseCommands xs b
			    else a:parseCommands xs b
    where (a,b) = command (x,s)

command:: (String,State)->(String,State)
command ("move",s) = ("", move s)
command ("mark",s) = ("", mark s)
command ("cw",s)   = ("", cw s)
command ("ccw",s)  = ("", ccw s)

-- The various colors
command ("black", s)   = ("", addColor s black)
command ("red", s)     = ("", addColor s red)
command ("green", s)   = ("", addColor s green)
command ("yellow", s)  = ("", addColor s yellow)
command ("blue", s)    = ("", addColor s blue)
command ("magenta", s) = ("", addColor s magenta)
command ("cyan", s)    = ("", addColor s cyan)
command ("white", s)   = ("", addColor s white)

command ("trans", s) = ("", addTrans s transparent)
command ("opaque", s)      = ("", addTrans s opaque)

command ("line", s)  = (makeLine s, s)
command ("fill", s)  = (makeFill s, s)

-- Pass through anything we don't recognize
command (x,s) = (x,s)



type Coord = Int
type Pos = (Coord, Coord)

type Component = Int
type Color = (Component, Component, Component)
type Transparency = Int
type Pixel = (Color,Transparency)

type Bitmap = [[Pixel]]
type Bitmaps = [Bitmap]

-- Direction Type(N,S,E,W)
type Dir = Char
type State = (Pos, Dir, Pos, (Color,Int), (Transparency,Int), Bitmaps)

-- Takes care of rotating the direction
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
cw :: State -> State
cw (a,b,c,d,e,f) = (a,cwDir b,c,d,e,f)
ccw :: State -> State
ccw (a,b,c,d,e,f) = (a,ccwDir b,c,d,e,f)

-- Mark(Copies current position into the marked position)
mark :: State -> State
mark (p, d, m, c, t, b) = (p, d, p, c, t, b)

-- Move(Updates position based on direction)
move :: State -> State
move (p, d, m, c, t, b) = (moveDir p d, d, m, c, t, b)
moveDir :: Pos->Dir->Pos
moveDir (x,y) d | d=='N' = (x, (y + 599) `mod` 600)
		| d=='S' = (x, (y +   1) `mod` 600)
		| d=='W' = ((x + 599) `mod` 600, y)
		| d=='E' = ((x +   1) `mod` 600, y)

addColor :: State->Color-> State
addColor (p, d, m, c, t, b) color = (p, d, m, (updateColor c color), t, b)

addTrans :: State->Transparency->State
addTrans (p, d, m, c, t, b) trans = (p, d, m, c, updateTrans t trans, b)

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

strColor :: Color->Transparency->String
strColor (r,g,b) a = "color " ++ show (r,g,b,a)

makeLine :: State->String
makeLine (p1,_,p2,(c,_),(t,_),_) = "line from" ++ (show p1) ++ " to " ++ (show p2) ++ "; " ++ (strColor c t)

makeFill :: State->String
makeFill (p,_,_,(c,_),(t,_),_) = "fill at " ++ (show p) ++ "; " ++ (strColor c t)