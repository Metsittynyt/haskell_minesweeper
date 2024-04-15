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
    translate (-144) (-144) $
      scale 32 32 $
        pictures
          [ case safeGetCell (board gameState) (x, 9 - y) of
              Just cell ->
                translate (fromIntegral x) (fromIntegral y) $
                  color (chooseColor cell) $
                    rectangleSolid 0.9 0.9
              Nothing -> blank -- if the cell is out of bounds, draw nothing
            | x <- [0 .. 9],
              y <- [0 .. 9]
          ]

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
  EventKey (MouseButton LeftButton) Down _ mousePos -> do
    -- Translate mouse position and scale it according to the cell size
    let (mouseX, mouseY) = invertMouseCoordinates mousePos

    -- Calculate the column and row in the grid from the mouse position
    let (col, row) = (floor mouseX, floor mouseY)

    -- Debug information
    putStrLn $ "Clicked pixel coordinates: " ++ show mousePos
    putStrLn $ "Converted to grid coordinates (col, row): " ++ show (col, row)

    -- Check if the click is within the bounds of the board and reveal the cell if it is
    let validMove = isValidMove (board gameState) (col, row)
    if validMove
      then return $ revealCell (col, row) gameState
      else return gameState -- Ignore if the move is invalid
  _ -> return gameState -- Ignore other events

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
      if isFlagged cell
        then gameState {board = updateCell x y (\cell -> cell {isFlagged = False}) brd}
        else gameState {board = updateCell x y (\cell -> cell {isFlagged = True}) brd}
    Nothing -> gameState

-- Function to check if the game is won
isGameWon :: GameState -> Bool
isGameWon (GameState brd _) =
  all cellRevealedOrMine brd
  where
    cellRevealedOrMine = all (\cell -> isRevealed cell || isMine cell)
