module Main where

import System.Random
import Data.List (intersperse)
import Text.Read (readMaybe)



-- Define the data structures
type Board = [[Cell]]
data Cell = Cell {
  isMine :: Bool,
  isRevealed :: Bool,
  adjacentMines :: Int
}
data GameState = GameState {
    board :: Board,
    gameStatus :: GameStatus
}
data GameStatus = Ongoing | Won | Lost deriving (Show, Eq)


-- Main function
main :: IO ()
main = do
    let initialBoard = initBoard 10 10
    minedBoard <- placeMines initialBoard 15
    let finalBoard = calculateAdjacency minedBoard
    let gameState = GameState finalBoard Ongoing
    mainLoop gameState


-- Initialize a game board with all cells hidden
initBoard :: Int -> Int -> Board
initBoard width height = replicate height (replicate width (Cell False False 0))


-- Place mines at random locations on the board
placeMines :: Board -> Int -> IO Board
placeMines brd 0 = return brd
placeMines brd numMines = do
    x <- randomRIO (0, width - 1)
    y <- randomRIO (0, height - 1)
    let newBoard = placeMine (x, y) brd -- Updated variable name to avoid shadowing
    placeMines newBoard (numMines - 1)
  where
    width = length (head brd)
    height = length brd

placeMine :: (Int, Int) -> Board -> Board
placeMine (xPos, yPos) currentBoard = updateCell xPos yPos (\cell -> cell { isMine = True }) currentBoard



-- Update a cell in the board
updateCell :: Int -> Int -> (Cell -> Cell) -> Board -> Board
updateCell x y f brd =
    let (beforeRows, targetRow:afterRows) = splitAt y brd
        (beforeCells, targetCell:afterCells) = splitAt x targetRow
        newCell = f targetCell
    in beforeRows ++ [beforeCells ++ [newCell] ++ afterCells] ++ afterRows


-- Main game loop
mainLoop :: GameState -> IO ()
mainLoop gameState = do
  case gameStatus gameState of
    Ongoing -> do
      putStrLn "Enter row and column to reveal, separated by space:"
      input <- getLine
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




-- Reveal a cell (simplified: does not handle cascading reveals or check for game end)
revealCell :: (Int, Int) -> GameState -> GameState
revealCell (x, y) gameState@(GameState brd status) =
  if inBounds (x, y) then
    let cell = (brd !! y) !! x
        newCell = cell { isRevealed = True } -- Reveal the cell.
        newBoard = updateCell x y (\_ -> newCell) brd
    in if isMine cell
       then GameState newBoard Lost -- Mark game as lost but with the cell revealed.
       else GameState newBoard status
  else gameState
  where
    width = length (head brd)
    height = length brd
    inBounds (x, y) = x >= 0 && y >= 0 && x < width && y < height




-- Print the game board to the console
printBoard :: Board -> IO ()
printBoard brd = mapM_ printRow brd
  where
    printRow row = putStrLn . concat . intersperse " " $ map showCell row
    showCell cell
      | isRevealed cell = if isMine cell then "*" else show (adjacentMines cell)
      | otherwise = "X"


-- Function to remove duplicates (used in placeMines)
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- Function to calculate the number of mines adjacent to each cell on the board
calculateAdjacency :: Board -> Board
calculateAdjacency brd = [[ calculateCellAdjacency x y | x <- [0..width-1]] | y <- [0..height-1]]
  where
    width = length (head brd)
    height = length brd
    calculateCellAdjacency x y = let
      cell = (brd !! y) !! x
      adjacentPositions = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], not (dx == 0 && dy == 0)]
      inBounds (x, y) = x >= 0 && y >= 0 && x < width && y < height
      countMines = length . filter (isMine . cellAt) . filter inBounds $ adjacentPositions
      cellAt (x, y) = (brd !! y) !! x
      in cell { adjacentMines = countMines }
