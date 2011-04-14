{-# LANGUAGE BangPatterns #-}
{-
 - Author: Keith Johnson, kjohns07@my.fit.edu
 - Course: CSE5400, Spring 2011
 - Project: endo3
-}



module Main where

import Text.Printf
import Data.Array.Diff
import Data.Int
import Data.Bits
import Data.List


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
type Bitmap = DiffUArray (Int,Int) Int32 -- bitwise store rgba(8bits each)
type Bitmaps = [Bitmap] -- List of bitmaps
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
initialState = ((0,0), 'E', (0,0), ((0,0,0),0), (opaque,0), bitmaps)
  where
    bitmaps = array ((0,0),(599,599)) [((x,y), packColor (black,transparent)) | x<-[0..599],y<-[0..599]] : []


packColor :: Pixel->Int32
packColor ((r,g,b),a) = fromIntegral (rb .|. gb .|. bb .|. ab)
  where
    rb = (r .&. 255) `shiftL` 24
    gb = (g .&. 255) `shiftL` 16
    bb = (b .&. 255) `shiftL` 8
    ab = (a .&. 255)

unpackColor :: Int32->Pixel
unpackColor i = ((r,g,b),a)
  where
    r = fromIntegral ((i `shiftR` 24) .&. 255)
    g = fromIntegral ((i `shiftR` 16) .&. 255)
    b = fromIntegral ((i `shiftR` 8) .&. 255)
    a = fromIntegral (i .&. 255)

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
parseCommands [] s = dumpImage s
parseCommands (x:xs) s = parseCommands xs b
    where (_,b) = command (x,s)

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
command ("line", s)  = (makeLine s, drawLine s)
command ("fill", s)  = (makeFill s, doFill s)

command ("add", s)    = ("", addBitmap s)
command ("compose",s) = ("", s)
command ("clip",s)    = ("", s)
--command ("compose",s) = trace "compose" ("", compose s) -- compose s
--command ("clip",s)    = trace "clip" ("", clip s) -- clip s

-- A special command to reset the bucket
command ("empty", s) = ("", resetBucket s)

-- Pass through anything we don't recognize
command (x,s) = (x,s)


-- Reset the state of a bucket(Clears colors/transparency)
resetBucket :: State -> State
resetBucket (p,d,m,_,_,b) = (p,d,m, ((0,0,0),0),(opaque,0),b)

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
 - We store the total value for each color component, and add each new component
 - when we add a color.  We also keep track of how many colors we have added, so
 - that we can get the final color value when necessary by simply dividing
 -}
addColor :: State->Color-> State
addColor (p, d, m, c, t, b) color = (p, d, m, (updateColor c color), t, b)

{- Transparency works the same as the color method above, but it only operates
 - on the single alpha channel
 -}
addTrans :: State->Transparency->State
addTrans (p, d, m, c, t, b) trans = (p, d, m, c, updateTrans t trans, b)

-- These two functions just update the total value and length
updateColor:: (Color,Int)->Color->(Color,Int)
updateColor (x,0) c = (c,1)
updateColor ((r,g,b),l) (ar,ag,ab) = ((r+ar,g+ag,b+ab),l+1)

updateTrans :: (Transparency,Int)->Transparency->(Transparency,Int)
updateTrans (_,0) t  = (t,1)
updateTrans (t,l) nt = ((t+nt), l+1)

-- Formats a Color/Transparency as "color (r,g,b,a)" for printing
strColor :: (Color,Int)->(Transparency,Int)->String
-- First 3 handle cases where one of the lengths is zero(i.e. no division)
strColor ((r,g,b),0) (a,0) = "color " ++ show (r,g,b,a)
strColor ((r,g,b),0) (a,lt) = "color " ++ show (r,g,b,div a lt)
strColor ((r,g,b),lc) (a,0) = "color " ++ show (div r lc, div g lc, div b lc, a)
-- Divide the component by the length to get the average
strColor ((r,g,b),lc) (a,lt) = "color " ++ show (div r lc,div g lc,div b lc,div a lt)

getColor :: (Color,Int)->Color
getColor ((r,g,b),0) = (r,g,b)
getColor ((r,g,b),lc) = (div r lc, div g lc, div b lc)

getTrans :: (Transparency,Int)->Transparency
getTrans (a,0)  = a
getTrans (a,lt) = div a lt

getPackedColor :: (Color,Int)->(Transparency,Int)->Int32
getPackedColor color_info trans_info = packColor
    (
      (
        floor ((fromIntegral r) * mult),
        floor ((fromIntegral g) * mult),
        floor ((fromIntegral b) * mult)
      ),
      a
    )
      where
        (r,g,b) = getColor color_info
        a = getTrans trans_info
        mult = (fromIntegral a) / 255

{-
  Adds a new bitmap to the list of bitmaps if there is room
-}
addBitmap :: State->State
addBitmap (p, d, m, c, t, b) | length b >=10 = (p, d, m, c, t, b)
                             | otherwise = (p, d, m, c, t, newB:b)
    where
        newB =  array ((0,0),(599,599)) [((x,y), packColor (black,transparent)) | x<-[0..599],y<-[0..599]]


{- Uses the current known state, generate a proper "line" command with the
 - correct coordinates and color value
 -}
makeLine :: State->String
makeLine (p1,_,p2,c,t,_) = "line from" ++ (show p1) ++ " to " ++ (show p2) ++ "; " ++ (strColor c t)

-- Same basic idea of makeLine; uses the current state to print a fill command
makeFill :: State->String
makeFill (p,_,_,c,t,_) = "fill at " ++ (show p) ++ "; " ++ (strColor c t)

{- Draws a line from p1 to p2 -}
drawLine :: State->State
drawLine (p1,d,p2,c,t,(b:bs)) = (p1,d,p2,c,t,(renderLine b (getPackedColor c t) p1 p2):bs)

{- Draw the actual line onto the bitmap -}
renderLine :: Bitmap->Int32->Pos->Pos->Bitmap
renderLine b p (x1,y1) (x2,y2) = newB
  where
     newB = b//(findLineCoords (dx,dy) (x,y) d d p [((x2,y2),p)])
       where
        dx = x2-x1
        dy = y2-y1
        d  = max (abs dx) (abs dy)
        c = if dx*dy <= 0
            then 1
            else 0
        z  =  floor (((fromIntegral d) - (fromIntegral c)) / 2)
        x  = x1 * d + z
        y  = y1 * d + z

findLineCoords :: Pos->Pos->Int->Int->Int32->[(Pos,Int32)]->[(Pos,Int32)]
findLineCoords dxy     xy    d 0  p l = l
findLineCoords (dx,dy) (x,y) d d2 p l = findLineCoords (dx,dy) ((x+dx),(y+dy)) d (d2-1) p (cpixel:l)
    where
      xd = floor ((fromIntegral x) / (fromIntegral d))
      yd = floor ((fromIntegral y) / (fromIntegral d))
      cpixel = ((xd,yd),p)

doFill :: State->State
doFill (pos, dir, mark, color_info, trans_info, (bmp:bmps)) = (pos, dir, mark, color_info, trans_info, newbmp:bmps)
    where
        newbmp = if search /= fillColor
          then
            floodFill pos fillColor search bmp
          else
            bmp
        fillColor = getPackedColor color_info trans_info
        search = bmp!pos

floodFill :: Pos->Int32->Int32->Bitmap->Bitmap
floodFill p@(x,y) fpx spx b
  | x<0 || y<0 || x>=600 || y>=600  = b -- Stop if out of bounds
  | b!p /= spx                      = b -- Stop if current pixel != search pixel
  | otherwise = ret
  where
    ret = floodFill (x+1,y) fpx spx $
          floodFill (x-1,y) fpx spx $
          floodFill (x,y+1) fpx spx $
          floodFill (x,y-1) fpx spx (b//[(p,fpx)])



-- Checks if the given position is within the image bounds (0..599)
inBounds :: Pos -> Bool
inBounds (x,y) = x < 100 && y < 100 && x >= 0 && y >= 0

compose :: State->State
--compose (p, d, m, c, t, (b1:b2:bs)) = (p, d, m, c, t, b1:bs)
compose (p, d, m, c, t, (b1:b2:bs)) = (p, d, m, c, t, (composeBitmap b1 b2):bs)
compose s = s

{- Takes in two bitmaps, and composes one onto another, returning the resulting bitmap -}
composeBitmap :: Bitmap->Bitmap->Bitmap
composeBitmap b1 b2 = b1 // uplist
  where
    uplist = [(p, composeHelper (b1!p) (b2!p)) | p<-(indices b1)]

composeHelper:: Int32->Int32->Int32
composeHelper (!p0) (!p1) = packColor
    (
      (
        r0 + floor ((fromIntegral r1) * mult),
        g0 + floor ((fromIntegral g1) * mult),
        b0 + floor ((fromIntegral b1) * mult)
      ),
      a0 + floor ((fromIntegral a1) * mult)
    )
      where
        ((r0,g0,b0),a0) = unpackColor p0
        ((r1,g1,b1),a1) = unpackColor p1
        mult = ((255 - (fromIntegral a0)) / 255)

clip :: State->State
clip (p, d, m, c, t, (b1:b2:bs)) = (p, d, m, c, t, (clipBitmap b1 b2):bs)
clip s = s

{- Takes in two bitmaps, and clips one onto another, returning the resulting bitmap -}
clipBitmap :: Bitmap->Bitmap->Bitmap
clipBitmap b1 b2 = b1 // uplist
  where
    uplist = [(p, clipHelper (b1!p) (b2!p)) | p<-(indices b1)]

clipHelper :: Int32->Int32->Int32
clipHelper p0 p1 = packColor
    (
      (
        floor ((fromIntegral r1) * mult),
        floor ((fromIntegral g1) * mult),
        floor ((fromIntegral b1) * mult)
      ),
      floor ((fromIntegral a1) * mult)
    )
      where
        ((r0,g0,b0),a0) = unpackColor p0
        ((r1,g1,b1),a1) = unpackColor p1
        mult = (fromIntegral a0) / 255

{- Converts the first bitmap into the a string in the imagemagick format -}
dumpImage :: State->[String]
dumpImage (p,d,m,c,t,b:bs) = "# ImageMagick pixel enumeration: 600,600,255,rgba":(bitmapToString b)

bitmapToString :: Bitmap->[String]
bitmapToString bitmap = [
        (printf "%d,%d: (%d, %d, %d, %d)" x y r g b a) |
            x<-[0..599],
            y<-[0..599],
            let ((r,g,b),a) = unpackColor (bitmap!(x,y))
        ]