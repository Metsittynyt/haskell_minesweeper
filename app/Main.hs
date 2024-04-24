module Main where

import System.Random ()
import Data.List ()
import Text.Read ()
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)


import Board ()
import GameState
    ( GameState, initialGameState, updateGame, handleEvent )  -- Import GameState type here
import Input ()
import Draw (drawGame)
import Button (drawButtons, handleButtonEvent)


-- Main function
main :: IO ()
main = do
    -- Initiate the game state.
    gameState <- initialGameState

    -- Setup the display window
    let windowSize = 10 * 32 + 2 * 40
    let window = InWindow "Minesweeper" (windowSize, windowSize + 100) (100, 100)

    -- Run the game using playIO from Gloss
    playIO window white 30 gameState drawAll handleAllEvents updateGame

-- Drawing function that combines game and button graphics
drawAll :: GameState -> IO Picture
drawAll gameState = do
  gamePic <- drawGame gameState  -- Draw the game state
  let buttonPics = drawButtons gameState  -- Draw buttons from Button.hs
  return $ pictures [gamePic, buttonPics]

-- Handling all game events, including button interactions
handleAllEvents :: Event -> GameState -> IO GameState
handleAllEvents event gameState = do
  gameState' <- handleButtonEvent event gameState  -- First handle button events
  handleEvent event gameState'  -- Then handle general game events
