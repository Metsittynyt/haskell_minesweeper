module Draw where

import Graphics.Gloss
import Numeric (showFFloat)
import GameState (GameState(..), GameStatus(..))
import Board (Cell(..), isMine, isRevealed, adjacentMines, safeGetCell)
import Button (drawButtons)

-- Function to render the game state
drawGame :: GameState -> IO Picture
drawGame gameState = return $
    pictures [
      translate (-144) (-194) $  -- Adjust the translation to center the board
        scale 32 32 $            -- Scale each cell to 32x32 pixels
          pictures [
            case safeGetCell (board gameState) (x, 9 - y) of
              Just cell ->
                translate (fromIntegral x) (fromIntegral y) $
                  pictures [
                    color (chooseColor cell) $ rectangleSolid 0.9 0.9,
                    drawCellText cell
                  ]
              Nothing -> blank
            | x <- [0 .. 9], y <- [0 .. 9]
          ],
      drawTimer (elapsedTime gameState),
      drawButtons gameState
    ]

-- Function to render the timer
drawTimer :: Float -> Picture
drawTimer time =
    translate (-160) 160 $
    scale 0.15 0.15 $
    color black $
    text $ "Time: " ++ showFFloat (Just 1) time "s"  -- Correctly format the floating point number


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

