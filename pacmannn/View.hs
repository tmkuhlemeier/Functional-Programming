-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Model
import MzMap

view :: GameState -> IO Picture
view gstate | runstatus gstate == GameOver = do
                                          writeFile "HighScores.txt" (show (score gstate) ++ "\n")
                                          allHighScores <- readFile "HighScores.txt"
                                          let highScores = lines allHighScores
                                          return $ pictures [scale 0.75 0.75 $ viewPure gstate, translate (-600) 300 $ scale 0.2 0.2 $ color green (text (head highScores))]
            | otherwise = return $ scale 0.75 0.75 $ viewPure gstate

pacPict :: Pacman -> Picture
pacPict (Pac x y _ _ (Closed _ )) = translate x y $ pacClosed
pacPict (Pac x y North _ (Open _ )) = translate x y $ rotate (-90) pacOpen
pacPict (Pac x y East _ (Open _ )) = translate x y $ pacOpen
pacPict (Pac x y South _ (Open _ )) = translate x y $ rotate 90 pacOpen
pacPict (Pac x y West _ (Open _ )) = translate x y $ rotate 180 pacOpen

pacOpen :: Picture
pacOpen = scale 0.95 1.1 $ pictures [color yellow $ circleSolid 11, color black $ polygon [(-4,0),(11,9),(11,-9)]]

pacClosed :: Picture
pacClosed = color yellow $ circleSolid 11

blinkPict :: Blinky -> Picture
pinkPict  :: Pinky -> Picture
inkPict   :: Inky -> Picture
clydPict  :: Clyde -> Picture


blinkPict (Blink x y _ _) = translate x y $ color red (rectangleSolid 20 20)
pinkPict (Pink x y _ _)   = translate x y $ color magenta (rectangleSolid 20 20)
inkPict (Ink x y _ _)     = translate x y $ color cyan (rectangleSolid 20 20)
clydPict (Clyd x y _ _)   = translate x y $ color orange (rectangleSolid 20 20)

drawMaze :: Maze -> Picture
drawMaze (MkMaze pc b p i c mzm) = pictures [drawObj, drawAgents]
  where
    drawAgents  = pictures [pacPict pc, blinkPict b, pinkPict p, inkPict i, clydPict c]
    drawObj     = pictures $ concat $ objMap mzm 0 0

objMap :: [String] -> Float -> Float -> [[Picture]]
objMap [] _ _     = []
objMap (r:rs) x y = (charMap r x y) : objMap rs 0 (y+1)

charMap :: String -> Float -> Float -> [Picture]
charMap [] _ _ = []
charMap (' ':cs) x y = (translate (x*30 -375) (y*30 -375) $ color black (rectangleSolid 30 30)) : charMap cs (x+1) y
charMap ('#':cs) x y = (translate (x*30 -375) (y*30 -375) $ color blue (rectangleSolid 30 30)) : charMap cs (x+1) y
charMap ('.':cs) x y = (translate (x*30 -375) (y*30 -375) $ pictures [color black (rectangleSolid 30 30), color white (circleSolid 3)]) : charMap cs (x+1) y
charMap ('E':cs) x y = (translate (x*30 -375) (y*30 -375) $ pictures [color black (rectangleSolid 30 30), color white (thickCircle 7 7)]) : charMap cs (x+1) y
charMap ('-':cs) x y = (translate (x*30 -375) (y*30 -375) $ pictures [color black (rectangleSolid 30 30), color white (rectangleSolid 30 5)]) : charMap cs (x+1) y

drawScore :: Score -> Picture
drawScore s = pictures [translate 400 300 $ scale 0.2 0.2 $ color white (text ("score:")), translate 400 250 $ scale 0.3 0.3 $ color white (text (show s))]

drawLives :: Lives -> Picture
drawLives One = translate 410 200 pacOpen
drawLives Two = pictures [translate 410 200 pacOpen, translate 440 200 pacOpen]
drawLives Three = pictures [translate 410 200 pacOpen, translate 440 200 pacOpen, translate 470 200 pacOpen]

drawRunStatus :: RunStatus -> Picture
drawRunStatus Paused = translate 400 150 $ scale 0.2 0.2 $ color green (text ("game paused"))
drawRunStatus Start = translate 400 150 $ scale 0.2 0.2 $ color red (text ("press 's' to start"))
drawRunStatus Dead = translate 400 150 $ scale 0.2 0.2 $ color black (text ("game paused"))
drawRunStatus Victory = translate 400 150 $ scale 0.2 0.2 $ color green (text ("victory!"))
drawRunStatus _ = translate 400 150 $ scale 0.2 0.2 $ color black (text ("running"))

viewPure :: GameState -> Picture
viewPure gstate = pictures [drawMaze (maze gstate), drawScore $ score gstate, drawLives $ lives gstate, drawRunStatus $ runstatus gstate]