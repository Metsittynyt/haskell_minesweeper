module Main where

import System.Random
import Data.List (intersperse)
import Text.Read (readMaybe)
import Graphics.Gloss.Interface.IO.Game

import Board
import GameState
import Input

-- Main function
main :: IO ()
main = do
    -- Welcome message and quick guide on how to play.
    putStrLn "Welcome to Minesweeper!"
    putStrLn ""
    putStrLn "The board size is 10 x 10. There are total of 10 bombs in hiding."
    putStrLn "You reveal cells by giving column and row number, separated by space."
    putStrLn "Column an row numbers go from 0 to 9."

    -- Initiate the game state.
    gameState <- initialGameState

    -- Setup the display window
    let windowSize = 10 * 32 + 2 * 40
    let window = InWindow "Minesweeper" (windowSize, windowSize + 100) (100, 100)
    -- Run the game using playIO from Gloss
    playIO window white 30 gameState drawGame handleEvent updateGame
