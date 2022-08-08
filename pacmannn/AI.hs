module AI where

import Model
import System.Random
import System.IO.Unsafe
import MzMap

aidirectionblinky :: Pacman -> Blinky -> MazeMap -> Blinky
aidirectionblinky (Pac pacx pacy dirpac _ _) (Blink gosx gosy dirgos state) x  | vision pacx pacy gosx gosy x = Blink gosx gosy (toPac pacx pacy gosx gosy dirgos) state  
                                                                     | otherwise              = Blink gosx gosy (newdir dirgos gosx gosy x) state
 
aidirectionpinky :: Pacman -> Pinky -> MazeMap -> Pinky
aidirectionpinky (Pac pacx pacy dirpac _ _) (Pink gosx gosy dirgos state) x  | vision pacx pacy gosx gosy x = Pink gosx gosy (toPac pacx pacy gosx gosy dirgos) state  
                                                                    | otherwise              = Pink gosx gosy (newdir dirgos gosx gosy x) state
 
aidirectioninky :: Pacman -> Inky -> MazeMap -> Inky
aidirectioninky (Pac pacx pacy dirpac _ _) (Ink gosx gosy dirgos state) x  | vision pacx pacy gosx gosy x = Ink gosx gosy (toPac pacx pacy gosx gosy dirgos) state  
                                                                   | otherwise              = Ink gosx gosy (newdir dirgos gosx gosy x) state
 
aidirectionclyde :: Pacman -> Clyde -> MazeMap -> Clyde
aidirectionclyde (Pac pacx pacy dirpac _ _) (Clyd gosx gosy dirgos state) x  | vision pacx pacy gosx gosy x = Clyd gosx gosy (toPac pacx pacy gosx gosy dirgos) state  
                                                                   | otherwise              = Clyd gosx gosy (newdir dirgos gosx gosy x) state

splitI :: Float -> Float  -> Float -> MazeMap -> Axis -> Bool
splitI x y z walls Horizontal | x < y = f True x y walls 
                              | x >= y = f True y x walls 
                      where
                        f bol st ut walls | cellVal st == cellVal ut = bol
                                          | st > 800 = False
                                          | otherwise = f (bol && not(notInWall (st,z) walls)) (st + 30) ut walls
splitI x y z walls Vertical | x < y = f True x y walls 
                            | x >= y = f True y x walls 
                      where
                        f bol st ut walls | cellVal st == cellVal ut = bol
                                          | st > 800 = False
                                          | otherwise = f (bol && not(notInWall (z,st) walls)) (st + 30) ut walls

 
toPac :: Px -> Py -> Px -> Py -> Direction -> Direction
toPac x1 y1 x2 y2 dir | x1 < x2 = West
                      | x1 > x2 = East
                      | y1 < y2 = South
                      | y2 < y1 = North
                      | otherwise = dir

cellVal :: Float -> Int
cellVal x = round ((x + 375) / 30)

notInWall :: (Px,Py) -> MazeMap -> Bool
notInWall (a,b) walls = blockVal /= '#' 
  where 
    blockVal = walls!!(cellVal b)!!(cellVal a)

midPt :: Float -> Bool
midPt c = abs (fromIntegral (cellVal c) * 30 - (c + 375)) <= 1 -- c `mod` 30 == 0 -- als het precies samenvalt met een stappgrootte

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East  = West
opposite West  = East 

vision :: Px -> Py -> Px -> Py -> MazeMap -> Bool
vision x1 y1 x2 y2 walls | x1 == x2  = splitI y1 y2 x1 walls Horizontal
                         | y1 == y2  = splitI x1 x2 y1 walls Vertical
                         | otherwise = False

newdir :: Direction -> Px -> Py -> MazeMap -> Direction -- The Ghost can't go backwards unless seeing pacman, this is to avoid ghosts getting stuck in a forward, backward loop
newdir x px py z | x == North = unsafePerformIO (pickrandom (possible [North,East,West] px py z) (opposite x))
                 | x == South = unsafePerformIO (pickrandom (possible [South,East,West] px py z) (opposite x))
                 | x == East  = unsafePerformIO (pickrandom (possible [East,North,South] px py z) (opposite x))
                 | x == West  = unsafePerformIO (pickrandom (possible [West,North,South] px py z) (opposite x))

possible :: [Direction] -> Px -> Py -> MazeMap -> [Direction] --The same as usual, change the 1 for the step size per tick
possible [] _ _ _ = []
possible (x:xs) x1 y1 y           | x == North && notInWall (x1, y1 + 18) y && midPt x1 = x : possible xs x1 y1 y
                                  | x == South && notInWall (x1, y1 - 18) y && midPt x1 = x : possible xs x1 y1 y
                                  | x == East  && notInWall (x1 + 18, y1) y && midPt y1 = x : possible xs x1 y1 y
                                  | x == West  && notInWall (x1 - 18, y1) y && midPt y1 = x : possible xs x1 y1 y
                                  | otherwise                                           = possible xs x1 y1 y

bfsdir :: Px -> Py -> MazeMap -> Direction
bfsdir px py walls = bfsdir1 [((px,py),(1000,1000))] [] where
        bfsdir1 active seen | inseen (400,400) seen = todir (traceback (400,400) seen) px py
                            | otherwise = let x = concat (map (neighbours . f) active) where
                                                f (a,b) = a 
                                                in  bfs x where
                                                bfs neigh = bfsdir1 (foldr yes [] neigh) (active ++ seen) where
                                                        yes (a,b) newact| inseen a seen = newact
                                                                        | notInWall a walls = newact
                                                                        | otherwise = (a,b) : newact
                                
traceback :: (Px,Py) -> [((Px,Py),(Px,Py))] -> [(Px,Py)]
traceback (400,400) seen = []
traceback x seen = foldr f x seen : traceback (foldr f x seen) seen where
        f (a,b) c | a == x = b
                  | otherwise = c

todir :: [(Px,Py)] -> Px -> Py -> Direction
todir (x:(x1,y1):y) px py | py == y1 - 5 = North
                          | py == y1 + 5 = South
                          | px == x1 + 5 = West
                          | px == x1 - 5 = East

inseen :: (Px,Py) -> [((Px,Py),(Px,Py))] -> Bool
inseen a b = foldr f False b where
        f (c,d) bol = c == a || bol

neighbours :: (Px,Py) -> [((Px,Py),(Px,Py))]
neighbours (x, y) = ((x + 5,y),(x,y)) : ((x - 5, y),(x,y)) : ((x, y+5),(x,y)) : ((x , y-5),(x,y)) : []

-------
--IO:--
-------

pickrandom :: [Direction] -> Direction -> IO Direction
pickrandom [] y = return y
pickrandom x y= do randomNumber <- randomIO
                   let z = x !! (randomNumber `mod` length x)
                   return z
