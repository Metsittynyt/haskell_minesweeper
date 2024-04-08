module GameState (
    GameState(..),
    GameStatus(..),
    revealCell
) where

import Board  -- Make sure Board exports necessary functionalities like updateCell, isMine, etc.

data GameState = GameState {
    board :: Board,
    gameStatus :: GameStatus
}

data GameStatus = Ongoing | Won | Lost deriving (Show, Eq)

-- Reveal a cell; simplified version.
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