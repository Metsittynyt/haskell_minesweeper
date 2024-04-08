module GameState (
    GameState(..),
    GameStatus(..),
    revealCell,
    isGameWon
) where

import Board

data GameState = GameState {
    board :: Board,
    gameStatus :: GameStatus
}

data GameStatus = Ongoing | Won | Lost deriving (Show, Eq)

-- Function to reveal cell and do possible flood fill
revealCell :: (Int, Int) -> GameState -> GameState
revealCell coords@(x, y) gameState@(GameState brd status) =
  case safeGetCell brd coords of
    Just cell -> if isRevealed cell
                 then gameState -- No action needed if already revealed.
                 else
                   let newCell = cell { isRevealed = True }
                       newBoard = safeSetCellRevealed brd coords
                   in if isMine cell
                      then gameState { board = newBoard, gameStatus = Lost } -- Update to Lost if it's a mine.
                      else if adjacentMines cell == 0
                           then let updatedState = gameState { board = newBoard, gameStatus = status }
                                    adjCoords = adjacentCoords coords (length brd) (length $ head brd)
                                    recursivelyRevealedState = foldl (flip revealCell) updatedState adjCoords
                                in recursivelyRevealedState
                           else gameState { board = newBoard, gameStatus = status }
    Nothing -> gameState -- Out of bounds, return state unchanged.


-- Function to check if the game is won
isGameWon :: GameState -> Bool
isGameWon (GameState brd _) =
    all cellRevealedOrMine brd
  where
    cellRevealedOrMine = all (\cell -> isRevealed cell || isMine cell)
