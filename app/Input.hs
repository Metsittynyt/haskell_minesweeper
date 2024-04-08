module Input (
    parseInput,
    isQuitCommand,
    isValidMove
) where

import Text.Read (readMaybe)
import Board

-- Parses the user input into a coordinate pair (Int, Int)
-- Returns Nothing for invalid inputs and Just (row, col) for valid inputs
parseInput :: String -> Maybe (Int, Int)
parseInput input = case map readMaybe (words input) :: [Maybe Int] of
    [Just row, Just col] -> Just (row, col)
    _ -> Nothing

-- Checks if the input string is a command to quit the game
isQuitCommand :: String -> Bool
isQuitCommand input = input == "quit"

-- Validates if the provided move is within the bounds of the board
isValidMove :: Board -> (Int, Int) -> Bool
isValidMove board (row, col) =
    let (maxRows, maxCols) = boardSize board
    in row >= 0 && col >= 0 && row < maxRows && col < maxCols
