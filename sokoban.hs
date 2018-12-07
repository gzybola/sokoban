{-# LANGUAGE OverloadedStrings #-} 
import CodeWorld
import Data.Text

replace:: Coord -> Coord -> [Coord] -> [Coord]
replace _ _ [] = []
replace y z (x:xs)
  | x == y           = z:xs
  | otherwise      = x:Main.replace y z xs

wall, ground, storage, box, startScreen, winnerScreen, closedMazes, saneMazes :: Picture
wall    = colored blue (solidRectangle 1 1)
ground  = colored purple (solidRectangle 1 1)
storage = colored orange (solidCircle 0.3) & ground
box     = colored yellow      (solidRectangle 1 1)
closedMazes = (pictureOfBools (Prelude.map isClosed badMazes)) 
saneMazes = pictureOfBools (Prelude.map isSane badMazes) 
winnerScreen = scaled 1 1 (lettering "Wygrales Sokobana!")
startScreen = scaled 1 1 (lettering "Sokoban!")

completedLevelScreen :: Int -> Picture
completedLevelScreen steps = lettering (Data.Text.append ("Poziom ukończony, liczba ruchów: ") (Data.Text.pack (show steps)))

data Maze = Maze Coord (Coord -> Tile) 
data Tile = Wall | Ground | Storage | Box | Blank deriving (Enum, Eq)
data Direction = R | U | L | D deriving (Enum, Eq)
data Coord = C Integer Integer
data SSState world = StartScreen | Running world 
data WithUndo a = WithUndo a [a]

instance Eq State where
  S mazes1 direction1 boxes1 steps1 == S mazes2 direction2 boxes2 steps2
        = (Prelude.length mazes1) == (Prelude.length mazes2) 
          && direction1 == direction2 
          && boxes1 == boxes2 
          && steps1 == steps2

mazes :: [Maze]
mazes = [goodMaze, goodMaze1] 
badMazes :: [Maze]
badMazes = [badMaze1, badMaze, goodMaze, goodMaze1, badMaze2]

instance Eq Coord where
  (C s1 s2) == (C s3 s4) = s1 == s3 && s2 == s4 
  
data Interaction world = Interaction
    world
    (Double -> world -> world)
    (Event -> world -> world)
    (world -> Picture)  
   
data State = S [Maze] Direction [Coord] Int
initialBoxes :: Maze -> [Coord]
initialBoxes (Maze start maze) = Prelude.filter (\x -> maze x == Box) attainable
      where 
             attainable = [C x y | x<-[-10..10], y<-[-10..10], reachable (C x y) start neighbours]
             neighbours coord = if canMoveOn (maze coord) then (mapList(\d -> adjacentCoord d coord) [R ..]) else []
initialState :: State
initialState = S mazes U (initialBoxes (Prelude.head mazes)) 0

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

player :: Direction -> Picture
player R     = colored pink (solidPolygon [(-1/2,1/2),(-1/2,-1/2),(1/2,0)])
player L     = rotated pi (player R)
player U     = rotated (0.5*pi) (player R)
player D     = rotated (1.5*pi)(player R)

etap4 :: Picture
etap4 = (translated (-5) 8 (lettering "Sane mazes"))
      & (translated 5 8 (lettering "Closed mazes"))
      & (translated (-5) 3 saneMazes)
      & (translated 5 3 closedMazes)
      & startScreen

removeBoxes :: (Coord -> Tile) -> Coord -> Tile
removeBoxes mazeFunction = f . mazeFunction 
  where 
  f :: Tile -> Tile
  f tile
    | tile == Box = Ground
    | otherwise  = tile 

addBoxes :: [Coord] -> (Coord -> Tile) -> Coord -> Tile
addBoxes boxes mazeFunction = f
  where 
  f x
    | elem x boxes = Box
    | otherwise = mazeFunction x

pictureOfMaze :: [Coord] -> (Coord -> Tile) -> Picture
pictureOfMaze boxes maze = drawNtimes (\r -> drawNtimes (\c -> drawTileAt (C r c) boxes maze) 10) 10 

drawNtimes :: (Integer -> Picture) -> Integer -> Picture
drawNtimes something m = go m
  where
    go :: Integer -> Picture
    go (-1) = blank
    go n  = something n & go (n-1) & something (-1*n)

drawTileAt :: Coord -> [Coord] -> (Coord -> Tile) -> Picture
drawTileAt (C r c) boxes maze = translated (fromIntegral r) (fromIntegral c) 
                            (drawTile (mazeBox maze boxes (C r c)))
   
mazeBox :: (Coord -> Tile) -> [Coord] -> Coord -> Tile
mazeBox f boxes = addBoxes boxes (removeBoxes f)   
   
 
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord d (C x y) 
  | d == R                    = C (x + 1) y
  | d == L                    = C (x - 1) y
  | d == D                    = C x (y - 1)
  | otherwise                = C x (y + 1)

handleTime :: Double -> world -> world
handleTime _ c = c

canMoveOn :: Tile -> Bool
canMoveOn tile
     | tile == Ground               = True
     | tile == Storage              = True 
     | otherwise                   = False
     
isWinning :: State -> Bool
isWinning (S [] _ _ _)                                  = False
isWinning (S ((Maze _ maze):_) _ boxes _) = f boxes
  where 
    f::[Coord] -> Bool
    f [] = True
    f (x:xs) = (maze x) == Storage && f xs
    
move :: Direction -> State -> State
move _ (S [] _ _ _) = initialState
move direction (S ((Maze playerCoord maze):rest) _ boxes steps)  
    | canMoveOn firstTile     = S ((Maze newPlayerCoord maze):rest) direction boxes (steps + 1)
    | firstTile == Box && canMoveOn secondTile = S ((Maze newPlayerCoord maze):rest) direction newBoxes (steps + 1)
    | otherwise              = S ((Maze playerCoord maze):rest) direction boxes steps
    where 
      firstTile = mazeBox maze boxes newPlayerCoord
      secondTile = mazeBox maze boxes nextBoxCoord
      newPlayerCoord = adjacentCoord direction playerCoord
      nextBoxCoord = adjacentCoord direction newPlayerCoord
      newBoxes = Main.replace newPlayerCoord nextBoxCoord boxes

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (S ((Maze playerCoord maze):rest) direction boxes steps)
   | isWinning state = (S rest direction (initialBoxes (Prelude.head rest)) 0)
   | key == "Right"             = move R state
   | key == "Up"                = move U state
   | key == "Left"              = move L state
   | key == "Down"              = move D state
   where state = (S ((Maze playerCoord maze):rest) direction boxes steps)
    
handleEvent _ w      = w

draw :: State -> Picture
draw (S [] _ _ _) = winnerScreen
draw (S ((Maze playerCoord maze):restMazes) direction boxes steps)
  |  winner                                    = completedLevelScreen steps
  | otherwise                                 = atCoord playerCoord (player direction) & pictureOfMaze boxes maze
  where winner = isWinning (S ((Maze playerCoord maze):restMazes) direction boxes steps)

resettable :: Interaction s -> Interaction s
resettable (Interaction w t e d) = Interaction  w t (enhancedHandleEvent w e) d

enhancedHandleEvent ::  world ->(Event -> world -> world) -> Event -> world -> world
enhancedHandleEvent i _ (KeyPress "Esc") _ = i
enhancedHandleEvent _ f e w = f e w

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw1)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = etap4
    draw' (Running s) = draw1 s
    
withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 step handle draw) = Interaction state0' step' handle' draw' 
  where
    state0' = WithUndo state0 []
    
    step' t (WithUndo s stack) = WithUndo (step t s) stack
    
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []         -> WithUndo s []
    handle' e  (WithUndo s stack)
       | s' == s                      = WithUndo s stack
       | otherwise                   = WithUndo (handle e s) (s:stack)
      where s' = handle e s
      
    draw' (WithUndo s _) = draw s

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle1 draw1) = interactionOf state0 step handle1 draw1

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k /2) (fromIntegral k) (go 0 xs)
  where n = Prelude.length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)
        
walk :: IO()
walk = runInteraction (withStartScreen (resettable (withUndo (Interaction initialState handleTime handleEvent draw))))

main::IO()
main = walk

--------------MAZES-------------------
goodMaze :: Maze 
goodMaze = Maze start maze 
  where
  start = C 1 1
  maze (C x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground
  
badMaze :: Maze 
badMaze = Maze start maze
  where
    start = C 1 1
    maze (C x y)
      | abs x > 4  || abs y > 2  = Blank
      | abs x == 4 || abs y == 2 = Wall
      | x ==  2 && y <= 0        = Wall
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0        = Box
      | otherwise                = Ground
  
goodMaze1 :: Maze
goodMaze1 = Maze start maze 
  where
    start = C 1 1
    maze (C x y)
      | abs x > 4  || abs y > 3  = Blank
      | abs x == 4 || abs y == 3 = Wall
      | x ==  2 && y <= 0        = Wall
      | x ==  -1 && y <= -1       = Wall
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0 && x <= 0 = Box
      | otherwise                = Ground
  
badMaze1 :: Maze
badMaze1 = Maze start maze
  where
    start = C 1 1
    maze (C x y)
      | abs x > 4  || abs y > 6  = Blank
      | abs x == 4 || abs y == 3 = Wall
      | x ==  2 && y <= 0        = Wall
      | abs y == 6               = Wall 
      | x ==  -1 && y <= -1       = Wall
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0 && x <= 1 = Box
      | otherwise                = Ground
      
badMaze2 :: Maze
badMaze2 = Maze start maze
  where
    start = C 1 1
    maze (C x y)
      | x == 2 && y == 3          = Blank
      | abs x > 4  || abs y > 6  = Blank
      | abs x == 4 || abs y == 3 = Wall
      | x ==  2 && y <= 0        = Wall
      | abs y == 6               = Blank 
      | x ==  -1 && y <= -1       = Wall
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0 && x <= 1 = Box
      | otherwise                = Ground

-----------LIST POLYMORPHIC FUNCTIONS--------------
elemList :: Eq a => a -> [a] -> Bool
elemList x (el:rest)
  | x == el      = True
  | otherwise  = elemList x rest
elemList _ [] = False

appendList :: [a] -> [a] -> [a]
appendList l1 l2 = foldList (\el l -> el : l) l2 l1

listLength :: [a] -> Integer
listLength list = foldList (\_ len -> len + 1) 0 list

filterList :: (a -> Bool) -> [a] -> [a]
filterList f list = foldList(\el l -> if f el then el:l else l) [] list

nth :: [a] -> Integer -> a
nth [] _ = error "too short list"
nth (el:rest) num 
  | num == 0 = el
  | otherwise = nth rest (num - 1)

mapList :: (a -> b) -> [a] -> [b]
mapList f list = foldList(\el l -> (f el):l) [] list  

andList :: [Bool] -> Bool
andList list = foldList (&&) True list

allList :: (a -> Bool) -> [a] -> Bool
allList p list = andList (mapList p list)

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList _ z [] = z
foldList f z (el : xs) = f el (foldList f z xs)

-----------GRAPHS---------------------

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = allList isOk vertises where
    vertises = dfs initial []
    dfs v visited = foldList (\vertice list -> if elem vertice list 
              then list else dfs vertice list) (v:visited) (neighbours v)
              
reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = not (isGraphClosed initial neighbours (\w -> w /= v))

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList (\v -> reachable v initial neighbours) vs


isClosed :: Maze -> Bool
isClosed (Maze initialCoord maze) = isBlankAttainable && initialOk
        where
          initialOk = canMoveOn (maze initialCoord)
          isBlankAttainable = isGraphClosed initialCoord neighbours (\coord -> maze coord /= Blank)
          neighbours coord = if canMoveOn (maze coord) then (mapList(\d -> adjacentCoord d coord) [R ..]) else []

isSane :: Maze -> Bool
isSane (Maze initialCoord maze) = boxes <= storages 
        where 
        boxes = listLength (filterList (\x -> maze x == Box) attainable)
        storages = listLength (filterList (\x -> maze x == Storage) attainable)
        attainable = [C x y | x<-[-10..10], y<-[-10..10], reachable (C x y) initialCoord neighbours]
        neighbours coord = if canMoveOn (maze coord) then (mapList(\d -> adjacentCoord d coord) [R ..]) else []
