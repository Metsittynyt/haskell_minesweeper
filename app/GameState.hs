module GameState
    ( GameState (..),
      GameStatus (..),
      GameScreen(..),
      initialGameState,
      initializeGame,
      revealCell,
      isGameWon,
      handleEvent,
      updateGame,)
where


import Board
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data GameState = GameState
  { board :: Board,
    gameStatus :: GameStatus,
    gameScreen :: GameScreen,
    elapsedTime :: Float,
    numRows :: Int,
    numCols :: Int,
    numMines :: Int,
    cellSize :: Int,
    windowWidth :: Int,
    windowHeight :: Int
  }

data GameStatus = Playing | Won | Lost | Paused  deriving (Show, Eq)
data GameScreen = Menu | Game deriving (Show, Eq)

-- Default game state setup
initialGameState :: IO GameState
initialGameState = do
  putStrLn "Initialize default game..."
  let rows = 10
      cols = 10 
      mines = 10
      cell_Size = 32
  let initialBoard = initBoard cols rows
  minedBoard <- placeMines initialBoard mines
  let finalBoard = calculateAdjacency minedBoard
  return GameState {
      board = finalBoard,
      gameStatus = Paused,
      gameScreen = Menu,
      elapsedTime = 0,
      numRows = rows,
      numCols = cols,
      numMines = mines,
      cellSize = cell_Size,
      windowWidth = 400,
      windowHeight = 500
  }

-- Initialize game based on parameters
initializeGame :: GameState -> IO GameState
initializeGame gs = do
    putStrLn $ "Initializing game with " ++ show (numRows gs) ++ " rows, " ++ show (numCols gs) ++ " cols, and " ++ show (numMines gs) ++ " mines."
    let rows = numRows gs
        cols = numCols gs
        mines = numMines gs
        ww = windowWidth gs
        wh = windowHeight gs
        cell_Size = floor $ calculateCellSize ww wh rows cols
    let initialBoard = initBoard cols rows
    minedBoard <- placeMines initialBoard mines
    let finalBoard = calculateAdjacency minedBoard
    return gs {
        board = finalBoard,
        gameStatus = Paused,
        elapsedTime = 0,
        cellSize = cell_Size
    }



-- Handle events and proceed accordingly
handleEvent :: Event -> GameState -> IO GameState
handleEvent event gameState@(GameState _ status _ _ _ _ _ _ _ _) =
    case status of
        Paused -> handlePausedState event gameState
        _      -> handleActiveState event gameState

-- When game is paused
handlePausedState :: Event -> GameState -> IO GameState
handlePausedState event gameState@(GameState _ _ screen _ _ _ _ _ _ _) =
    case (event, screen) of
        (EventKey (Char 'p') Down _ _, _) -> 
            return $ gameState { gameStatus = if gameStatus gameState == Paused then Playing else Paused }
        _ -> return gameState


-- When game is active
handleActiveState :: Event -> GameState -> IO GameState
handleActiveState event gameState@(GameState brd status screen elapsedTime _ _ _ _ _ _) = case event of
  -- Handle the pause/resume toggle
  EventKey (Char 'p') Down _ _ ->  -- Pressing 'p' will pause/resume the game
    return $ gameState { gameStatus = if status == Paused then Playing else Paused }

  -- Handle left mouse button click (reveal cell).
  EventKey (MouseButton LeftButton) Down _ mousePos -> do
    let (col, row) = convertMouseCoords gameState mousePos  -- Pass gameState here
    let validMove = isValidMove (board gameState) (col, row)
    if validMove
      then return $ revealCell (col, row) gameState
      else return gameState -- Ignore if the move is invalid

  -- Handle right mouse button click (toggle flag).
  EventKey (MouseButton RightButton) Down _ mousePos -> do
    let (col, row) = convertMouseCoords gameState mousePos  -- Pass gameState here
    let validMove = isValidMove (board gameState) (col, row)
    if validMove
      then return $ toggleFlag (col, row) gameState
      else return gameState -- Ignore if the move is invalid
  _ -> return gameState -- Handle other events


-- Helper function to convert screen coordinates to grid coordinates
convertMouseCoords :: GameState -> Point -> (Int, Int)
convertMouseCoords gameState (x, y) =
  let (gridX, gridY) = invertMouseCoordinates gameState (x, y)
   in (floor gridX, floor gridY)

-- Function to convert the screen coordinates of the mouse to grid coordinates
invertMouseCoordinates :: GameState -> Point -> (Float, Float)
invertMouseCoordinates gameState (screenX, screenY) =
  let (xOffset, yOffset) = calculateOffsets gameState
      (gridX, gridY) = (screenX + xOffset, yOffset - screenY)
   in (gridX / fromIntegral (cellSize gameState), gridY / fromIntegral (cellSize gameState))

-- Function to calculate the offsets for drawing the board
-- Calculate offsets for centering the board on the screen
calculateOffsets :: GameState -> (Float, Float)
calculateOffsets gameState =
  let cellSize' = fromIntegral (cellSize gameState)
      xOffset = (fromIntegral (numCols gameState) * cellSize') / 2
      yOffset = ((fromIntegral (numRows gameState) * cellSize') / 2 ) - 50
  in (xOffset, yOffset)



-- Update the game state over time
updateGame :: Float -> GameState -> IO GameState
updateGame timeStep gameState@(GameState brd status screen elapsedTime _ _ _ _ _ _) =
  return $ case status of
    Playing -> gameState { elapsedTime = elapsedTime + timeStep }
    _ -> gameState  -- Do not update time if paused, won, lost, or exiting

-- Validates if the provided move is within the bounds of the board
isValidMove :: Board -> (Int, Int) -> Bool
isValidMove board (x, y) = 
  y >= 0 && y < length board && x >= 0 && x < length (head board)

-- Function to reveal cell and do possible flood fill
revealCell :: (Int, Int) -> GameState -> GameState
revealCell coords@(x, y) gameState@(GameState brd  _ _ _ _ _ _ _ _ _) =
  case safeGetCell brd coords of
    Just cell -> if isRevealed cell
                 then gameState  -- No action needed if already revealed.
                 else
                   let newBoard = safeSetCellRevealed brd coords
                       newState = gameState { board = newBoard }
                   in if isMine cell
                      then newState { gameStatus = Lost }  -- Game is lost if a mine is revealed.
                      else if adjacentMines cell == 0
                           then let adjCoords = adjacentCoords coords (length $ head brd) (length brd)
                                    recursivelyRevealedState = foldl (flip revealCell) newState adjCoords
                                in if isGameWon recursivelyRevealedState
                                   then recursivelyRevealedState { gameStatus = Won }  -- Check if game is won.
                                   else recursivelyRevealedState
                           else if isGameWon newState
                                then newState { gameStatus = Won }  -- Check if game is won.
                                else newState
    Nothing -> gameState  -- Out of bounds, return state unchanged.

-- Toggle flag
toggleFlag :: (Int, Int) -> GameState -> GameState
toggleFlag (x, y) gameState@(GameState brd  _ _ _ _ _ _ _ _ _) =
  case safeGetCell brd (x, y) of
    Just cell ->
      if not (isRevealed cell) -- Check if the cell is not revealed
        then
          let newBoard = updateCell x y toggleFlagState brd
           in gameState {board = newBoard}
        else gameState -- If the cell is revealed, do not change the state
    Nothing -> gameState -- If the cell is out of bounds, do not change the state
  where
    toggleFlagState cell = cell {isFlagged = not (isFlagged cell)}

-- Function to check if the game is won
isGameWon :: GameState -> Bool
isGameWon (GameState brd _ _ _ _ _ _ _ _ _) =
  all cellsCorrect brd
  where
    cellsCorrect = all cellCorrect  -- Check every cell in each row
    cellCorrect cell
      | isMine cell = not (isRevealed cell) || isFlagged cell -- Mines should not be revealed or should be flagged
      | otherwise = isRevealed cell -- Non-mines should be revealed
