module Main where

import System.Random
import Data.List (intersperse)
import Text.Read (readMaybe)

import Board
import GameState
import Input (parseInput)

main :: IO ()
main = do
    putStrLn "Welcome to Minesweeper!"
    let initialBoard = initBoard 10 10
    minedBoard <- placeMines initialBoard 15
    let finalBoard = calculateAdjacency minedBoard
    let gameState = GameState finalBoard Ongoing
    mainLoop gameState

mainLoop :: GameState -> IO ()
mainLoop gameState = do
  case gameStatus gameState of
    Ongoing -> do
      putStrLn "Enter row and column to reveal, separated by space:"
      input <- getLine
      if input == "quit" then do
        putStrLn "Game over. Thanks for playing!"
        return ()
      else do
        let parsedInput = map readMaybe . words $ input :: [Maybe Int]
        case parsedInput of
          [Just row, Just col] -> do
            let newState = revealCell (col, row) gameState
            printBoard (board newState) -- Print the updated board.
            case gameStatus newState of
              Lost -> putStrLn "Boom! You've hit a mine. Game over."
              _    -> mainLoop newState
          _ -> putStrLn "Invalid input. Please enter valid row and column numbers separated by a space."
    Won -> putStrLn "Congratulations! You've won the game!"
    Lost -> putStrLn "Unexpected game over." -- This case might now be redundant.
    