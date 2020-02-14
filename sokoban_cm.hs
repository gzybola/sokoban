import System.IO

-------------DATA & TYPES--------------------------
data Coord = C Integer Integer
data Interaction world = Interaction
    world
    (Event -> world -> world)
    (world -> Screen)
    
data Event = KeyPress String
type Screen = String
type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun

data SSState world = StartScreen | Running world 
data State = S {
        mazesList  ::[Maze],
        stateDirection :: Direction,
        stateBoxes :: [Coord],
        stepCoutner :: Int
}
data Direction = R | U | L | D deriving (Enum, Eq)
data Maze = Maze Coord (Coord->Tile)
data Tile = Wall | Ground | Storage | Box | Blank deriving (Enum, Eq)
(&) :: (b -> c) -> (a -> b) -> a -> c
(&) = (.)
data WithUndo a = WithUndo a [a]

instance Eq Coord where
  (C s1 s2) == (C s3 s4) = s1 == s3 && s2 == s4 
instance Eq State where
  S mazes1 direction1 boxes1 steps1 == S mazes2 direction2 boxes2 steps2
        = (Prelude.length mazes1) == (Prelude.length mazes2) 
          && direction1 == direction2 
          && boxes1 == boxes2 
          && steps1 == steps2

----------------DRAWING------------------------------
translated :: Integer -> Integer -> Picture -> Picture
translated x y pic = translate2 x y blank & pic & translate x y blank  
        where translate x1 y1 pic1 f = pic1 (\a b -> f (x1 + a) (y1 + b))  
              translate2 x1 y1 pic1 f = pic1 (\a b -> f (-x1 + a) (-y1 +b))
 
drawTileAt :: Coord -> [Coord] -> (Coord -> Tile) -> Picture 
drawTileAt (C x y) boxes maze =  translated x y (drawTile (mazeBox maze boxes (C x y)))

draw :: State -> Screen 
draw (S [] _ _ _) = drawPicture( (lettering ("KONIEC GRY")))
draw (S ((Maze (C playerCoordx playerCoordy) maze):restMazes) direction boxes steps)
      | winning           =  drawPicture( (lettering ("Poziom ukończony, liczba ruchów: " ++ (show steps))))
      | otherwise         =  drawPicture finalPicture 
         where
          pictures = (map (\c -> drawTileAt c boxes maze) coords)
          finalPicture = translated playerCoordx playerCoordy player & foldl (&) blank (pictures)
          winning = isWinning (S ((Maze (C playerCoordx playerCoordy) maze):restMazes) direction boxes steps)
          coords = [(C x y) | y <-[12, 11..(-12)], x<-[-40..40]]

drawPicture :: Picture -> Screen
drawPicture pic = map render coords
        where
          render (C 40 _) = '\n'
          render (C x y) = (pic (\_ _ -> ' ')) x y    
          coords = [(C x y) | y <-[12, 11..(-12)], x<-[-40..40]]

lettering :: String -> Picture
lettering [] = blank
lettering text = foldr (&) blank (map letteringByCharacter [0..(length text - 1)]) 
  where
     letteringByCharacter :: Int -> Picture
     letteringByCharacter number = (translated (fromIntegral(number)) 0 letter)
       where
         letter:: Picture
         letter _ 0 0 = (text !! number)
         letter drawF x1 x2 = drawF x1 x2 

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral (k `div` 2)) (fromIntegral k) (go 0 xs)
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

        pictureOfBool True =  lettering "o"
        pictureOfBool False = lettering "*"

--------------- PICTURES-------------------------
blank, ground, wall, storage, box, player:: Picture

blank = id

wall _ 0 0 = '#'
wall drawF x y = drawF x y

storage _ 0 0 = 'o'
storage drawF x y = drawF x y 

box _ 0 0 = 'x'
box drawF x y = drawF x y

player _ 0 0 = '@'
player drawF x y = drawF x y

ground _ 0 0 = '-'
ground drawF x y = drawF x y 

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

closedMazes, saneMazes, startScreen, etap4 :: Picture
closedMazes = pictureOfBools (map isClosed badMazes) 
saneMazes = pictureOfBools (map isSane badMazes) 

startScreen =  (lettering "SOKOBAN")
etap4 = startScreen & (translated (-15) 0 (lettering "Sane mazes"))
      & (translated 15 0 (lettering "Closed mazes"))
      & (translated (-15) (-5) saneMazes)
      & (translated 15 (-5) closedMazes)

----------------BOX STUFF--------------------
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

mazeBox :: (Coord -> Tile) -> [Coord] -> Coord -> Tile
mazeBox f boxes = addBoxes boxes (removeBoxes f) 

initialBoxes :: Maze -> [Coord]
initialBoxes (Maze start maze) = Prelude.filter (\x -> maze x == Box) attainable
      where 
             attainable = [C x y | x<-[-10..10], y<-[-10..10], reachable (C x y) start neighbours]
             neighbours coord = if canMoveOn (maze coord) then (map(\d -> adjacentCoord d coord) [R ..]) else []

------------------MAZES-----------------------
goodMaze :: Maze 
goodMaze = Maze start maze 
  where
  start = C 1 1
  maze (C x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x == 3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise = Ground

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
      | otherwise = Ground

-----------------GRAPHS----------------------
isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = all isOk vertises where
    vertises = dfs initial []
    dfs v visited = foldr (\vertice list -> if elem vertice list 
              then list else dfs vertice list) (v:visited) (neighbours v)
              
reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = not (isGraphClosed initial neighbours (/= v))

isClosed :: Maze -> Bool
isClosed (Maze initialCoord maze) = isBlankAttainable && initialOk
        where
          initialOk = canMoveOn (maze initialCoord)
          isBlankAttainable = isGraphClosed initialCoord neighbours (\coord -> maze coord /= Blank)
          neighbours coord = if canMoveOn (maze coord) then (map(\d -> adjacentCoord d coord) [R ..]) else []

isSane :: Maze -> Bool
isSane (Maze initialCoord maze) = boxes <= storages 
        where 
          boxes = length (filter (\x -> maze x == Box) attainable)
          storages = length (filter (\x -> maze x == Storage) attainable)
          attainable = [C x y | x<-[-10..10], y<-[-10..10], reachable (C x y) initialCoord neighbours]
          neighbours coord = if canMoveOn (maze coord) then (map (\d -> adjacentCoord d coord) [R ..]) else []

--------------UNDO RESTART STARTSCREEN-------------------------
withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 handle draw0) = Interaction state0' handle' draw' 
  where
    state0' = WithUndo state0 []
    
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []         -> WithUndo s []
    handle' e  (WithUndo s stack)
       | s' == s                      = WithUndo s stack
       | otherwise                   = WithUndo (handle e s) (s:stack)
      where s' = handle e s
      
    draw' (WithUndo s _) = draw0 s

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0  handle draw1)
  = Interaction state0'  handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = (drawPicture etap4)
    draw' (Running s) = draw1 s

resettable :: Interaction s -> Interaction s
resettable (Interaction w e d) = Interaction w (enhancedHandleEvent w e) d

enhancedHandleEvent ::  world ->(Event -> world -> world) -> Event -> world -> world
enhancedHandleEvent i _ (KeyPress "Esc") _ = i
enhancedHandleEvent _ f e w = f e w

---------------EVENT HANDLING-----------------
adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord d (C x y) 
  | d == R                    = C (x + 1) y
  | d == L                    = C (x - 1) y
  | d == D                    = C x (y - 1)
  | otherwise = C x (y + 1)

replace:: Coord -> Coord -> [Coord] -> [Coord]
replace _ _ [] = []
replace y z (x:xs)
  | x == y           = z:xs
  | otherwise      = x:replace y z xs

canMoveOn :: Tile -> Bool
canMoveOn tile = tile `elem` [Ground, Storage]

move :: Direction -> State -> State
move _ (S [] _ _ _) = initialState
move direction state@(S ((Maze playerCoord maze):rest) _ boxes steps)  
    | canMoveOn firstTile                      = S newMazes direction boxes (steps + 1)
    | firstTile == Box && canMoveOn secondTile = S newMazes direction newBoxes (steps + 1)
    | otherwise                                = state { stateDirection = direction }
    where 
      firstTile = mazeBox maze boxes newPlayerCoord
      secondTile = mazeBox maze boxes nextBoxCoord
      newPlayerCoord = adjacentCoord direction playerCoord
      nextBoxCoord = adjacentCoord direction newPlayerCoord
      newBoxes = replace newPlayerCoord nextBoxCoord boxes
      newMazes = (Maze newPlayerCoord maze):rest


isWinning :: State -> Bool
isWinning (S [] _ _ _)                                  = False
isWinning (S ((Maze _ maze):_) _ boxes _) = all ((==Storage).maze ) boxes

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state
   | isWinning state        = nextMaze state
   | key == "d"             = move R state
   | key == "w"             = move U state
   | key == "a"             = move L state
   | key == "s"             = move D state
   where 
        nextMaze (S (_:[]) _ _ _) = S [] U [] 0
        nextMaze state2@(S [] _ _ _) = state2
        nextMaze(S (_:restMazes) direction _ _) 
                = (S restMazes direction (initialBoxes (Prelude.head restMazes)) 0)
    
handleEvent _ w = w

-----------------MAIN ------------------------
initialState :: State
initialState = S mazes U (initialBoxes (Prelude.head mazes)) 0
mazes :: [Maze]
mazes = [goodMaze, goodMaze1] 
badMazes :: [Maze]
badMazes = [badMaze1, badMaze, goodMaze, goodMaze1, badMaze2]

runInteraction:: Interaction s -> IO()
runInteraction (Interaction state0 handle drawFun) = do
        hSetBuffering stdout (BlockBuffering (Just 2025))
        hSetBuffering stdin NoBuffering         
        putStr (drawFun state0)
        go state0
        where
          go state' = do 
                letter <- getChar
                let 
                        newState = handle (KeyPress [letter]) state' 
                        in do
                            putStr "\ESCc"
                            putStr (drawFun (newState))
                            go newState 
main :: IO()
main = mainLoop
mainLoop :: IO()
mainLoop = do
runInteraction (withStartScreen(resettable(withUndo(Interaction initialState handleEvent draw)))) 

