module Main where

import System.Random ()
import Data.List ()
import Text.Read ()
import Graphics.Gloss.Interface.IO.Game

import Board ()
import GameState
    ( drawGame, handleEvent, initialGameState, updateGame )
import Input ()


-- Main function
main :: IO ()
main = do
    -- Initiate the game state.
    gameState <- initialGameState

    -- Setup the display window
    let windowSize = 10 * 32 + 2 * 40
    let window = InWindow "Minesweeper" (windowSize, windowSize + 100) (100, 100)

    -- Run the game using playIO from Gloss
    playIO window white 30 gameState drawGame handleEvent updateGame
