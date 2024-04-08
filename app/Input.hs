module Input (
    parseInput
) where

import Text.Read (readMaybe)

-- Parses the user input into a coordinate pair (Int, Int)
-- Returns Nothing for invalid inputs and Just (row, col) for valid inputs
parseInput :: String -> Maybe (Int, Int)
parseInput input = case map readMaybe (words input) :: [Maybe Int] of
    [Just row, Just col] -> Just (row, col)
    _ -> Nothing
