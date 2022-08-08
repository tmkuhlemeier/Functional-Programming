-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Model
import AI
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import MzMap

--move all agents in the level
moveAgents :: Maze -> Maze
moveAgents mz = mz {getPac = movePac (getPac mz) (getMazeMap mz),
                  getBl = moveBL (aidirectionblinky (getPac mz) (getBl mz) (getMazeMap mz)), 
                  getPink = movePink (aidirectionpinky (getPac mz) (getPink mz) (getMazeMap mz)), 
                  getInk = moveInk (aidirectioninky (getPac mz) (getInk mz) (getMazeMap mz)), 
                  getClyd = moveClyd (aidirectionclyde (getPac mz) (getClyd mz) (getMazeMap mz)) }


-- unfortunately, we couldn't finish this part
{-
deathupdate :: GameState -> GameState
deathupdate gstate {maze = x, lives = y , runstatus = z}| blinkpair (getPac x) (getBl x) = gstate {maze = initialMaze, lives = y - 1, runstatus = approve y}
                                                        | pinkpair (getPac x) (getPink x) = gstate {maze = initialMaze, lives = y - 1, runstatus = approve y}
                                                        | inkpair (getPac x) (getInk x) = gstate {maze = initialMaze, lives = y - 1, runstatus = approve y}
                                                        | clydepair (getPac x) (getClyd x) = gstate {maze = initialMaze, lives = y - 1, runstatus = approve y}
                                                        | otherwise = gstate

blinkpair :: Pacman -> Blinky -> Bool
blinkpair (Pac x y _ _ _) (Blink x1 y1 _ _) = x == x1 && y == y1

pinkpair :: Pacman -> Pinky -> Bool
pinkpair (Pac x y _ _ _) (Pink x1 y1 _ _) = x == x1 && y == y1

inkpair :: Pacman -> Inky -> Bool
inkpair (Pac x y _ _ _) (Ink x1 y1 _ _) = x == x1 && y == y1

clydepair :: Pacman -> Clyde -> Bool
clydepair (Pac x y _ _ _) (Clyde x1 y1 _ _) = x == x1 && y == y1

approve :: Float -> Runstatus
approve 1 = GameOver
approve x = Start 
-}

-- change the way the maze currently looks 
changeMaze :: Int -> Int -> Char -> MazeMap -> MazeMap
changeMaze x y newc rss = let (r1,r2:rs) = splitAt y rss in r1 ++ changeCell r2 : rs
  where
    changeCell css = let (c1,_:cs) = splitAt x css in c1 ++ newc : cs

moveBL :: Blinky -> Blinky
moveBL (Blink x y dir stat) | dir == North = Blink x (y + 5) dir stat
                            | dir == South = Blink x (y - 5) dir stat
                            | dir == East = Blink (x + 5) y dir stat
                            | dir == West = Blink (x - 5) y dir stat

movePink :: Pinky -> Pinky
movePink (Pink x y dir stat)| dir == North = Pink x (y + 5) dir stat
                            | dir == South = Pink x (y - 5) dir stat
                            | dir == East = Pink (x + 5) y dir stat
                            | dir == West = Pink (x - 5) y dir stat

moveInk :: Inky -> Inky
moveInk (Ink x y dir stat)  | dir == North = Ink x (y + 5) dir stat
                            | dir == South = Ink x (y - 5) dir stat
                            | dir == East = Ink (x + 5) y dir stat
                            | dir == West = Ink (x - 5) y dir stat

moveClyd :: Clyde -> Clyde
moveClyd (Clyd x y dir stat)| dir == North = Clyd x (y + 5) dir stat
                            | dir == South = Clyd x (y - 5) dir stat
                            | dir == East = Clyd (x + 5) y dir stat
                            | dir == West = Clyd (x - 5) y dir stat

--updates the maze and the scores
updateState :: GameState -> GameState
updateState gstate | blockVal == '.' = gstate {maze = ((maze gstate) {getMazeMap = newMaze}), score = score gstate + 10}
                   | blockVal == 'E' = gstate {maze = ((maze gstate) {getMazeMap = newMaze}), score = score gstate + 50}
                   | otherwise                      = gstate
  where
    mzm = getMazeMap $ maze gstate 
    pac = getPac $ maze gstate
    indexY (Pac _ y _ _ _) = cellVal y
    indexX (Pac x _ _ _ _)= cellVal x
    blockVal = mzm!!(indexY pac)!!(indexX pac)
    newMaze = changeMaze (indexX pac) (indexY pac) ' ' mzm

--can we go this direction?
safeDir :: Px -> Py -> Direction -> MazeMap -> Bool
safeDir x y North mzm = withinPath x (y + 15) mzm
safeDir x y East mzm  = withinPath (x + 15) y mzm
safeDir x y South mzm = withinPath x (y - 15) mzm
safeDir x y West mzm  = withinPath (x - 15) y mzm

--can we go further without bumping into something?
withinPath :: Float -> Float -> MazeMap -> Bool
withinPath x y mzm = blockVal == ' ' || blockVal == '.' || blockVal == 'E' 
  where 
    blockVal = mzm!!(cellVal y)!!(cellVal x)

--keep moving forward
stayForward :: Pacman -> Pacman
stayForward (Pac x y North mv j) = Pac x (y+5) North mv j
stayForward (Pac x y East mv j)  = Pac (x+5) y East mv j
stayForward (Pac x y South mv j) = Pac x (y-5) South mv j
stayForward (Pac x y West mv j)  = Pac (x-5) y West mv j

bite :: Pacman -> Pacman
bite (Pac x y dir mv (Open i))   | i >= 3 = Pac x y dir mv (Closed 0) --close mouth again
                                 | otherwise = Pac x y dir mv (Open $ i + 1) --wait a little longer
bite (Pac x y dir mv (Closed i)) | i >= 3 = Pac x y dir mv (Open 0) --open mouth again
                                 | otherwise = Pac x y dir mv (Closed $ i + 1)

--try to move pacman in the desired direction, else keep moving forward, and if that's not possible keep the same current position
movePac :: Pacman -> MazeMap -> Pacman
movePac pc@(Pac x y dir UpMv j) mzm    | midPt x && withinPath x (y + 30) mzm   = bite $ Pac x (y+5) North UpMv j
                                       | safeDir x y dir mzm                    = bite $ stayForward pc
                                       | otherwise                              = pc
movePac pc@(Pac x y dir RightMv j) mzm | midPt y && withinPath (x + 30) y mzm = bite $ Pac (x+5) y East RightMv j
                                       | safeDir x y dir mzm                    = bite $ stayForward pc
                                       | otherwise                              = pc
movePac pc@(Pac x y dir DownMv j) mzm  | midPt x && withinPath x (y - 30) mzm = bite $ Pac x (y-5) South DownMv j
                                       | safeDir x y dir mzm                    = bite $ stayForward pc
                                       | otherwise                              = pc
movePac pc@(Pac x y dir LeftMv j) mzm  | midPt y && withinPath (x - 30) y mzm = bite $ Pac (x-5) y West LeftMv j
                                       | safeDir x y dir mzm                    = bite $ stayForward pc
                                       | otherwise                              = pc


-- reads the key from the keyboard and decides what to do with it
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) gstate | runstatus gstate == Paused = gstate {runstatus = Running}
                                               | otherwise                  = gstate {runstatus = Paused}
inputKey (EventKey (Char 's') Down _ _) gstate | runstatus gstate == Start = gstate {runstatus = Running}
                                                          | otherwise                 = gstate
inputKey (EventKey (Char 'r') Down _ _) gstate = initialState
inputKey (EventKey (SpecialKey KeyUp) _ _ _) gstate = gstate {maze = changeDir KeyUp $ maze gstate}
inputKey (EventKey (SpecialKey KeyDown) _ _ _) gstate = gstate {maze = changeDir KeyDown $ maze gstate}
inputKey (EventKey (SpecialKey KeyLeft) _ _ _) gstate = gstate {maze = changeDir KeyLeft $ maze gstate}
inputKey (EventKey (SpecialKey KeyRight) _ _ _) gstate = gstate {maze = changeDir KeyRight $ maze gstate}
inputKey _ gstate                               = gstate -- Otherwise keep the same

changeDir :: SpecialKey -> Maze -> Maze
changeDir KeyUp mz@(MkMaze (Pac x y dir _ j) _ _ _ _ _)    = mz {getPac = (Pac x y dir UpMv j)}
changeDir KeyDown mz@(MkMaze (Pac x y dir _ j) _ _ _ _ _)  = mz {getPac = (Pac x y dir DownMv j)}
changeDir KeyLeft mz@(MkMaze (Pac x y dir _ j) _ _ _ _ _)  = mz {getPac = (Pac x y dir LeftMv j)}
changeDir KeyRight mz@(MkMaze (Pac x y dir _ j) _ _ _ _ _) = mz {getPac = (Pac x y dir RightMv j)}

-------
--IO:--
-------

step :: Float -> GameState -> IO GameState
step f gstate | runstatus gstate /= Running = return gstate
              | otherwise                   = return $ updateState $ gstate {maze = moveAgents $ maze gstate}

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)
