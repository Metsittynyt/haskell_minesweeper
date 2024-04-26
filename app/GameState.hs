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
import Input

data GameState = GameState
  { board :: Board,
    gameStatus :: GameStatus,
    gameScreen :: GameScreen,
    elapsedTime :: Float
  }

data GameStatus = Playing | Won | Lost | Paused  deriving (Show, Eq)
data GameScreen = Menu | Game deriving (Show, Eq)

-- Initial game state setup
initialGameState :: IO GameState
initialGameState = return $ GameState {
  board = [],
  gameStatus = Paused,
  gameScreen = Menu,
  elapsedTime = 0
}

-- Initialize game
initializeGame :: IO GameState
initializeGame = do
    putStrLn "Initializing new game..."
    let initialBoard = initBoard 10 10
    minedBoard <- placeMines initialBoard 10
    let finalBoard = calculateAdjacency minedBoard
    return GameState {
        board = finalBoard,
        gameStatus = Playing,
        gameScreen = Game,
        elapsedTime = 0
    }



-- Handle events and proceed accordingly
handleEvent :: Event -> GameState -> IO GameState
handleEvent event gameState@(GameState board status screen elapsedTime) =
    case status of
        Paused -> handlePausedState event gameState
        _      -> handleActiveState event gameState

-- When game is paused
handlePausedState :: Event -> GameState -> IO GameState
handlePausedState event gameState@(GameState _ _ screen _) =
    case (event, screen) of
        (EventKey (Char 'p') Down _ _, _) -> 
            return $ gameState { gameStatus = if gameStatus gameState == Paused then Playing else Paused }
        (EventKey (Char 'e') Down _ _, Menu) -> initializeGame
        _ -> return gameState


-- When game is active
handleActiveState  :: Event -> GameState -> IO GameState
handleActiveState  event gameState@(GameState brd status screen elapsedTime) = case event of
  -- Handle the pause/resume toggle
  EventKey (Char 'p') Down _ _ ->  -- Pressing 'p' will pause/resume the game
    return $ gameState { gameStatus = if status == Paused then Playing else Paused }

  -- Handle left mouse button click (reveal cell).
  EventKey (MouseButton LeftButton) Down _ mousePos -> do
    let (col, row) = convertMouseCoords mousePos
    let validMove = isValidMove (board gameState) (col, row)
    if validMove
      then return $ revealCell (col, row) gameState
      else return gameState -- Ignore if the move is invalid

  -- Handle right mouse button click (toggle flag).
  EventKey (MouseButton RightButton) Down _ mousePos -> do
    let (col, row) = convertMouseCoords mousePos
    let validMove = isValidMove (board gameState) (col, row)
    if validMove
      then return $ toggleFlag (col, row) gameState
      else return gameState -- Ignore if the move is invalid
  _ -> return gameState -- Handle other events

-- Helper function to convert screen coordinates to grid coordinates
convertMouseCoords :: Point -> (Int, Int)
convertMouseCoords (x, y) =
  let (gridX, gridY) = invertMouseCoordinates (x, y)
   in (floor gridX, floor gridY)

-- Function to convert the screen coordinates of the mouse to grid coordinates
invertMouseCoordinates :: Point -> (Float, Float)
invertMouseCoordinates (screenX, screenY) =
  let (gridX, gridY) = (screenX + 160, 110 - screenY)
   in (gridX / cellSize, gridY / cellSize)
  where
    cellSize = 32 -- Size of each cell

-- Update the game state over time
updateGame :: Float -> GameState -> IO GameState
updateGame timeStep gameState@(GameState brd status screen elapsedTime) =
  return $ case status of
    Playing -> gameState { elapsedTime = elapsedTime + timeStep }
    _ -> gameState  -- Do not update time if paused, won, lost, or exiting


-- Function to reveal cell and do possible flood fill
revealCell :: (Int, Int) -> GameState -> GameState
revealCell coords@(x, y) gameState@(GameState brd status screen elapsedTime) =
  case safeGetCell brd coords of
    Just cell -> if isRevealed cell
                 then gameState  -- No action needed if already revealed.
                 else
                   let newBoard = safeSetCellRevealed brd coords
                       newState = gameState { board = newBoard }
                   in if isMine cell
                      then newState { gameStatus = Lost }  -- Game is lost if a mine is revealed.
                      else if adjacentMines cell == 0
                           then let adjCoords = adjacentCoords coords (length brd) (length $ head brd)
                                    recursivelyRevealedState = foldl (flip revealCell) newState adjCoords
                                in if isGameWon recursivelyRevealedState
                                   then recursivelyRevealedState { gameStatus = Won }  -- Check if game is won.
                                   else recursivelyRevealedState
                           else if isGameWon newState
                                then newState { gameStatus = Won }  -- Check if game is won.
                                else newState
    Nothing -> gameState  -- Out of bounds, return state unchanged.


toggleFlag :: (Int, Int) -> GameState -> GameState
toggleFlag (x, y) gameState@(GameState brd status screen elapsedTime) =
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
isGameWon (GameState brd _ _ elapsedTime) =
  all cellsCorrect brd
  where
    cellsCorrect = all cellCorrect  -- Check every cell in each row
    cellCorrect cell
      | isMine cell = not (isRevealed cell) || isFlagged cell -- Mines should not be revealed or should be flagged
      | otherwise = isRevealed cell -- Non-mines should be revealed
