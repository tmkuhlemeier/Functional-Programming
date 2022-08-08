module Model where

import Data.Bool
import MzMap

data Lives = One | Two | Three

type Score = Int 

type Level = Int

data RunStatus = Start | Paused | Victory | Dead | Running | GameOver deriving(Eq)

type Px = Float

type Py = Float

data Axis = Horizontal | Vertical

type Width = Float

type Height = Float

type Length = Float

data Wall = MkWall Px Py Width Height

data Direction = North | East | South | West deriving(Eq)

data DesiredMove = UpMv | DownMv | LeftMv | RightMv

data Status = Normal | Dizzy Float | Eaten

type Ai = [Direction] -- A list of Directions which can be mapped to different states of the environment of a Ghost, so that the Ghost knows what direction to move in corresponding to which environment / or situation.

data Blinky = Blink Px Py Direction Status
data Pinky  = Pink Px Py Direction Status
data Inky   = Ink Px Py Direction Status
data Clyde  = Clyd Px Py Direction Status

data Jaw = Open Int | Closed Int

data Pacman = Pac Px Py Direction DesiredMove Jaw

data Maze = MkMaze { getPac :: Pacman, getBl :: Blinky, getPink :: Pinky, getInk :: Inky, getClyd :: Clyde, getMazeMap :: MazeMap}

data GameState = GameState { lives :: Lives, maze :: Maze, score :: Score
                 , level :: Level, runstatus :: RunStatus}

initialMaze :: Maze
initialMaze = MkMaze (Pac 0 (-75) East RightMv (Closed 0)) b p i c mazeMap
  where
      b = Blink (-15) 0 North Normal
      p = Pink 15 0 North Normal
      i = Ink 15 0 North Normal
      c = Clyd 0 15 North Normal

initialState :: GameState
initialState = GameState Three initialMaze 0 1 Start

