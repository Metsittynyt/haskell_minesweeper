module Board (
    Board,
    Cell(..),
    initBoard,
    placeMines,
    calculateAdjacency,
    updateCell,
    printBoard,
    boardSize,
    isCellRevealed
) where

import System.Random (randomRIO)
import Data.List (intersperse)

type Board = [[Cell]]

data Cell = Cell {
  isMine :: Bool,
  isRevealed :: Bool,
  adjacentMines :: Int
}
-- Initialize a game board with all cells hidden
initBoard :: Int -> Int -> Board
initBoard width height = replicate height (replicate width (Cell False False 0))

-- A function to get the dimensions of the board
boardSize :: Board -> (Int, Int)
boardSize brd = (length brd, length (head brd))

-- A function to access specific cell based on coordinates
isCellRevealed :: Board -> (Int, Int) -> Bool
isCellRevealed brd (x, y) = isRevealed ((brd !! y) !! x)

-- Place mines at random locations on the board
placeMines :: Board -> Int -> IO Board
placeMines brd 0 = return brd
placeMines brd numMines = do
    x <- randomRIO (0, length (head brd) - 1)
    y <- randomRIO (0, length brd - 1)
    let newBoard = placeMine (x, y) brd
    placeMines newBoard (numMines - 1)

placeMine :: (Int, Int) -> Board -> Board
placeMine (xPos, yPos) currentBoard = updateCell xPos yPos (\cell -> cell { isMine = True }) currentBoard

-- Update a cell in the board
updateCell :: Int -> Int -> (Cell -> Cell) -> Board -> Board
updateCell x y f brd =
    let (beforeRows, targetRow:afterRows) = splitAt y brd
        (beforeCells, targetCell:afterCells) = splitAt x targetRow
        newCell = f targetCell
    in beforeRows ++ [beforeCells ++ [newCell] ++ afterCells] ++ afterRows

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

-- Print the game board to the console with row and column indices
printBoard :: Board -> IO ()
printBoard brd = do
    let colNumbers = concatMap (\i -> " " ++ show i) [0..length (head brd) - 1]
    putStrLn (" " ++ colNumbers)  -- Adjusted: Added an extra space for alignment
    mapM_ printRow (zip [0..] brd)
  where
    printRow (idx, row) = do
        putStr (show idx ++ " ")  -- Print the row number before each row
        putStrLn . concat . intersperse " " $ map showCell row
    showCell cell
      | isRevealed cell = if isMine cell then "*" else show (adjacentMines cell)
      | otherwise = "X"
