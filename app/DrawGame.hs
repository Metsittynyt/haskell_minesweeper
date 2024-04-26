module DrawGame where

import Graphics.Gloss
import Button (Button(..), drawButtons)
import Numeric (showFFloat)
import GameState (GameState(..), GameStatus(..), GameScreen(..))
import Board (Cell(..), isMine, isRevealed, adjacentMines, safeGetCell)
import System.Exit (exitSuccess)
import Debug.Trace


-- Function to render the game state
drawGame :: GameState -> IO Picture
drawGame gameState = return $
    pictures [
      drawBoard gameState,
      drawTimer gameState (elapsedTime gameState),
      drawButtons gameState gameButtons,
      drawGameStatusMessage gameState
    ]

-- Draw board
drawBoard :: GameState -> Picture
drawBoard gameState =
  let cellSize' = fromIntegral (cellSize gameState)
      xOffset = - ((fromIntegral (numCols gameState) * cellSize' / 2) - cellSize' / 2)
      yOffset = - (50 + (fromIntegral (numRows gameState) * cellSize' / 2) - cellSize' / 2)
  in translate xOffset yOffset $
       scale cellSize' cellSize' $
         pictures [
            case safeGetCell (board gameState) (x, y) of
                Just cell ->
                    translate (fromIntegral x) (fromIntegral (numRows gameState - 1 - y)) $
                        pictures [
                            color (chooseColor cell) $ rectangleSolid 0.9 0.9,
                            drawCellText cell
                        ]
                Nothing -> blank
            | x <- [0 .. numCols gameState - 1], y <- [0 .. numRows gameState - 1]
        ]


-- Helper functions for drawing specific parts of a cell
drawCellText :: Cell -> Picture
drawCellText cell
  | isRevealed cell && not (isMine cell) && adjacentMines cell > 0 =
      translate (-0.2) (-0.2) $
        scale 0.005 0.005 $
          color black $
            text (show $ adjacentMines cell)
  | otherwise = blank

chooseColor :: Cell -> Color
chooseColor cell
  | isRevealed cell = if isMine cell then red else greyN 0.5
  | isFlagged cell = orange
  | otherwise = greyN 0.8

-- Function to render the timer
drawTimer :: GameState -> Float -> Picture
drawTimer gameState time =
    translate (-160) 160 $
    scale 0.15 0.15 $
    color black $
    text $ "Time: " ++ showFFloat (Just 1) time "s"

-- Draw 'game over' and 'you win' messages
drawGameStatusMessage :: GameState -> Picture
drawGameStatusMessage gameState
  | gameStatus gameState == Lost = translate (-60) 120 $ scale 0.15 0.15 $ color red $ text "Game Over!"
  | gameStatus gameState == Won = translate (-60) 120 $ scale 0.15 0.15 $ color orange $ text "You won!"
  | otherwise = blank  -- Return blank if neither condition is met


-- Define buttons for the game screen
gameButtons :: [Button]
gameButtons = [
    Button "New Game" (-110, 220) (100, 40) (const orange) backToMenu,
    Button "Pause" (0, 220) (100, 40) (const orange) togglePause,
    Button "Exit" (110, 220) (100, 40) (const orange) exitGame
    ]


-- Back to menu
backToMenu :: GameState -> IO GameState
backToMenu gs = do
  putStrLn "Back to menu..."
  return gs { gameScreen = Menu}

-- Function to toggle pause
togglePause :: GameState -> IO GameState
togglePause gs = do
  putStrLn "Toggle pause..."
  return gs { gameStatus = if gameStatus gs == Paused then Playing else Paused }

-- Exit game
exitGame :: GameState -> IO GameState
exitGame gs = do
  putStrLn "Exiting game..."
  exitSuccess
