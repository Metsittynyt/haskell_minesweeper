module GameState (
    GameState(..),
    GameStatus(..),
    revealCell
) where

import Board  -- Ensure Board exports necessary functionalities like updateCell, isMine, etc.

data GameState = GameState {
    board :: Board,
    gameStatus :: GameStatus
}

data GameStatus = Ongoing | Won | Lost deriving (Show, Eq)

-- Reveal a cell; simplified version.
revealCell :: (Int, Int) -> GameState -> GameState
revealCell (x, y) gameState@(GameState brd status) =
    let cell = (brd !! y) !! x
        newCell = cell { isRevealed = True } -- Reveal the cell.
        newBoard = updateCell x y (\_ -> newCell) brd
    in if isMine cell
       then gameState { board = newBoard, gameStatus = Lost } -- Mark game as lost but with the cell revealed.
       else gameState { board = newBoard, gameStatus = status }
