module Main where

import System.Random ()
import Data.List ()
import Text.Read ()
import Graphics.Gloss.Interface.IO.Game

import GameState
import DrawGame
import DrawMenu
import Button


main :: IO ()
main = do
    -- Initiate the game state, starting with the menu.
    gameState <- initialGameState

    -- Setup the display window
    let window = InWindow "Minesweeper" (400, 500) (100, 100)

    -- Run the game using playIO from Gloss
    playIO window white 30 gameState drawAll handleAllEvents updateGame

-- Drawing function that adapts based on game status
drawAll :: GameState -> IO Picture
drawAll gameState =
    case gameScreen gameState of
        Menu -> drawMenu gameState
        Game -> drawGame gameState


-- Handling all game events, including button interactions
handleAllEvents :: Event -> GameState -> IO GameState
handleAllEvents event gameState = case gameScreen gameState of
    Menu -> do
        gameState' <- handleButtonEvent event gameState (menuButtons ++ paramButtons)
        handleEvent event gameState'
    Game -> do
        gameState' <- handleButtonEvent event gameState gameButtons
        handleEvent event gameState'
    _ -> return gameState

