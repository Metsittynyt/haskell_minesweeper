module Board (
    Board,
    Cell(..),
    initBoard,
    placeMines,
    calculateAdjacency,
    adjacentCoords,
    printBoard,
    isCellRevealed,
    safeGetCell,
    safeSetCellRevealed,
    boardSize
) where

import System.Random (randomRIO)
import Data.List (intersperse)
import Data.Maybe (catMaybes)


-- Define the board and cell data structures.
type Board = [[Cell]]
data Cell = Cell {
  isMine :: Bool,
  isRevealed :: Bool,
  adjacentMines :: Int
} deriving (Eq, Show)

-- Initialize a game board with all cells hidden.
initBoard :: Int -> Int -> Board
initBoard width height = replicate height (replicate width (Cell False False 0))

-- Place mines at random locations on the board.
placeMines :: Board -> Int -> IO Board
placeMines brd numMines
  | numMines <= 0 = return brd
  | otherwise = do
      x <- randomRIO (0, length (head brd) - 1)
      y <- randomRIO (0, length brd - 1)
      if isMine ((brd !! y) !! x)
        then placeMines brd numMines -- Try again if we hit an already mined cell.
        else placeMines (updateCell x y (\cell -> cell { isMine = True }) brd) (numMines - 1)

-- Calculate the number of mines adjacent to each cell on the board.
calculateAdjacency :: Board -> Board
calculateAdjacency brd = [[ calculateCellAdjacency x y brd | x <- [0..width-1]] | y <- [0..height-1]]
  where
    width = length (head brd)
    height = length brd
    calculateCellAdjacency x y board =
      let cell = (board !! y) !! x
          adjCells = adjacentCoords (x, y) width height
          -- Use map and catMaybes to safely count mines without fromJust
          mineCount = length . filter id . catMaybes $ map (isMineCell board) adjCells
      in cell { adjacentMines = mineCount }

-- Helper function to check if a cell at given coords is a mine, safely.
isMineCell :: Board -> (Int, Int) -> Maybe Bool
isMineCell board coords = fmap isMine (safeGetCell board coords)

isCellRevealed :: Board -> (Int, Int) -> Bool
isCellRevealed board (x, y) = case safeGetCell board (x, y) of
    Just cell -> isRevealed cell
    Nothing -> False  -- Assume False if coordinates are out-of-bounds

-- Helper to get a list of adjacent cell coordinates.
adjacentCoords :: (Int, Int) -> Int -> Int -> [(Int, Int)]
adjacentCoords (x, y) maxWidth maxHeight =
  [(nx, ny) | nx <- [x-1..x+1], ny <- [y-1..y+1], nx >= 0, ny >= 0, nx < maxWidth, ny < maxHeight, not (nx == x && ny == y)]

-- Update a cell in the board.
updateCell :: Int -> Int -> (Cell -> Cell) -> Board -> Board
updateCell x y f brd =
  let (beforeRows, targetRow:afterRows) = splitAt y brd
      (beforeCells, targetCell:afterCells) = splitAt x targetRow
      newCell = f targetCell
  in beforeRows ++ [beforeCells ++ newCell : afterCells] ++ afterRows

-- Print the game board to the console with row and column indices.
printBoard :: Board -> IO ()
printBoard brd = do
    let colNumbers = concatMap (\i -> " " ++ show i) [0..length (head brd) - 1]
    putStrLn (" " ++ colNumbers)
    mapM_ printRow (zip [0..] brd)
  where
    printRow (idx, row) = do
        putStr (show idx ++ " ")
        putStrLn . concat . intersperse " " $ map showCell row
    showCell cell
      | isRevealed cell = if isMine cell then "*" else show (adjacentMines cell)
      | otherwise = "X"

-- Retrieve a cell safely based on coordinates.
safeGetCell :: Board -> (Int, Int) -> Maybe Cell
safeGetCell board (x, y)
  | y >= 0 && y < length board && x >= 0 && x < length (head board) = Just (board !! y !! x)
  | otherwise = Nothing

-- Set a cell's isRevealed flag to True safely.
safeSetCellRevealed :: Board -> (Int, Int) -> Board
safeSetCellRevealed board (x, y) =
  case safeGetCell board (x, y) of
    Just cell -> if not (isRevealed cell)
                 then updateCell x y (\_ -> cell { isRevealed = True }) board
                 else board
    Nothing -> board

-- Function to get the dimensions of the board.
boardSize :: Board -> (Int, Int)
boardSize brd = (length brd, length (head brd))
