module Main where

import System.Random
import Data.List (intersperse)
import Text.Read (readMaybe)

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

    -- Initiate the board and start the game.
    let initialBoard = initBoard 10 10
    minedBoard <- placeMines initialBoard 10
    let finalBoard = calculateAdjacency minedBoard
    let gameState = GameState finalBoard Ongoing
    mainLoop gameState


-- Main game loop
mainLoop :: GameState -> IO ()
mainLoop gameState = do
  case gameStatus gameState of
    Ongoing -> do
      putStrLn ""
      putStrLn "Enter column and row to reveal, or type 'quit' to exit: "
      input <- getLine
      -- Check if the input is a quit command.
      if isQuitCommand input then do
        putStrLn "Exiting game..."
        return ()
      else case parseInput input of
        Just coords -> 
          -- Check if the move is valid.
          if not (isValidMove (board gameState) coords) then do
            putStrLn "Input is out of bounds, please try again."
            mainLoop gameState
          else if isCellRevealed (board gameState) coords then do
            putStrLn "This cell is already revealed. Try another one."
            mainLoop gameState
          else do
            -- Proceed with revealing the cell if the move is valid.
            let newState = revealCell coords gameState
            printBoard (board newState)
            -- Check if the game is won
            if isGameWon newState then do
                putStrLn "Congratulations! You've won the game!"
            else case gameStatus newState of
              Lost -> putStrLn "Boom! You've hit a mine. Game over."
              _    -> mainLoop newState
        Nothing -> do
          putStrLn "Invalid input. Please enter valid row and column numbers separated by a space, or type 'quit' to exit."
          mainLoop gameState
    Won -> putStrLn "Congratulations! You've won the game!"
    Lost -> putStrLn "Unexpected game over."
