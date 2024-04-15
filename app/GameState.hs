module GameState
  ( GameState (..),
    GameStatus (..),
    revealCell,
    isGameWon,
    drawGame,
    handleEvent,
    updateGame,
  )
where

import Board
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Input

data GameState = GameState
  { board :: Board,
    gameStatus :: GameStatus
  }

data GameStatus = Ongoing | Won | Lost deriving (Show, Eq)

drawGame :: GameState -> IO Picture
drawGame gameState =
  return $
    translate (-144) (-144) $ -- Adjust the translation to center the board
      scale 32 32 $ -- Scale each cell to 32x32 pixels
        pictures
          [ case safeGetCell (board gameState) (x, 9 - y) of
              Just cell ->
                translate (fromIntegral x) (fromIntegral y) $
                  pictures
                    [ color (chooseColor cell) $ rectangleSolid 0.9 0.9, -- Draw the cell background
                      drawCellText cell -- Call function to draw the number of adjacent mines
                    ]
              Nothing -> blank -- if the cell is out of bounds, draw nothing
            | x <- [0 .. 9],
              y <- [0 .. 9]
          ]

-- Function to draw the number of adjacent mines on the cell
drawCellText :: Cell -> Picture
drawCellText cell
  | isRevealed cell && not (isMine cell) && adjacentMines cell > 0 =
      translate (-0.2) (-0.2) $ -- Adjust translation to center the text in the cell
        scale 0.005 0.005 $ -- Adjust scaling to fit the text size
          color black $ -- Set text color to black
            text (show $ adjacentMines cell) -- Convert the number of adjacent mines to text
  | otherwise = blank

chooseColor :: Cell -> Color
chooseColor cell
  | isRevealed cell =
      if isMine cell
        then red
        else
          if isFlagged cell
            then yellow
            else greyN 0.5
  | isFlagged cell = orange
  | otherwise = greyN 0.8

handleEvent :: Event -> GameState -> IO GameState
handleEvent event gameState = case event of
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
  let (gridX, gridY) = (screenX + 160, 160 - screenY)
   in (gridX / cellSize, gridY / cellSize)
  where
    cellSize = 32 -- Size of each cell
    windowWidth = 400 -- Width of the game window, update if it's different
    windowHeight = 400 -- Height of the game window, update if it's different

updateGame :: Float -> GameState -> IO GameState
updateGame _ gameState = return gameState -- No continuous updates needed

-- Function to reveal cell and do possible flood fill
revealCell :: (Int, Int) -> GameState -> GameState
revealCell coords@(x, y) gameState@(GameState brd status) =
  case safeGetCell brd coords of
    Just cell ->
      if isRevealed cell
        then gameState -- No action needed if already revealed.
        else
          let newCell = cell {isRevealed = True}
              newBoard = safeSetCellRevealed brd coords
           in if isMine cell
                then gameState {board = newBoard, gameStatus = Lost} -- Update to Lost if it's a mine.
                else
                  if adjacentMines cell == 0
                    then
                      let updatedState = gameState {board = newBoard, gameStatus = status}
                          adjCoords = adjacentCoords coords (length brd) (length $ head brd)
                          recursivelyRevealedState = foldl (flip revealCell) updatedState adjCoords
                       in recursivelyRevealedState
                    else gameState {board = newBoard, gameStatus = status}
    Nothing -> gameState -- Out of bounds, return state unchanged.

toggleFlag :: (Int, Int) -> GameState -> GameState
toggleFlag (x, y) gameState@(GameState brd status) =
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
isGameWon (GameState brd _) =
  all cellsCorrect brd
  where
    cellsCorrect row = all cellCorrect row -- Check every cell in each row
    cellCorrect cell
      | isMine cell = not (isRevealed cell) || isFlagged cell -- Mines should not be revealed or should be flagged
      | otherwise = isRevealed cell -- Non-mines should be revealed
