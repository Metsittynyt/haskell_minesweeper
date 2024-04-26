module DrawMenu where

import Graphics.Gloss
import System.Exit (exitSuccess)

import GameState (GameState(..), GameStatus(..), GameScreen(..), initializeGame)
import Button (Button(..), drawButtons, handleButtonEvent)

-- Draw the menu
drawMenu :: GameState -> IO Picture
drawMenu gameState = return $
    pictures [
        translate (-60) 200 $ scale 0.15 0.15 $ color black $ text "Minesweeper",
        drawButtons gameState (menuButtons ++ paramButtons),
        drawParameters gameState 
    ]

-- Define buttons for the menu screen
menuButtons :: [Button]
menuButtons = [
    Button "Play" (-10, -50) (100, 40) (const orange) startGame,
    Button "Exit" (-10, -100) (100, 40) (const red) exitGame
    ]

-- Draw changeable parameters with current values
drawParameters :: GameState -> Picture
drawParameters gameState =
    pictures [
        translate (-100) 150 $ scale 0.125 0.125 $ color black $ text "Mines: ",
        translate 30 150 $ scale 0.125 0.125 $ color black $ text $ show (numMines gameState),
        translate (-100) 90 $ scale 0.125 0.125 $ color black $ text "Rows: ",
        translate 30 90 $ scale 0.125 0.125 $ color black $ text $ show (numRows gameState),
        translate (-100) 30 $ scale 0.125 0.125 $ color black $ text "Columns: ",
        translate 30 30 $ scale 0.125 0.125 $ color black $ text $ show (numCols gameState)
    ]

-- Define buttons for adjusting parameters
paramButtons :: [Button]
paramButtons = [
    Button "+" (80, 150) (20, 20) (const orange) (\gs -> return $ adjustParams gs "mines" 1),
    Button "-" (0, 150) (20, 20) (const orange) (\gs -> return $ adjustParams gs "mines" (-1)),
    Button "+" (80, 90) (20, 20) (const orange) (\gs -> return $ adjustParams gs "rows" 1),
    Button "-" (0, 90) (20, 20) (const orange) (\gs -> return $ adjustParams gs "rows" (-1)),
    Button "+" (80, 30) (20, 20) (const orange) (\gs -> return $ adjustParams gs "cols" 1),
    Button "-" (0, 30) (20, 20) (const orange) (\gs -> return $ adjustParams gs "cols" (-1))
    ]


-- Button logics
-- Function to start a new game
startGame :: GameState -> IO GameState
startGame gs = do
  putStrLn "Starting a new game..."
  newGameState <- initializeGame gs
  return newGameState { gameScreen = Game }



-- Function to exit the game
exitGame :: GameState -> IO GameState
exitGame gs = do
    putStrLn "Exiting game..."
    exitSuccess

-- Function to adjust parameters
adjustParams :: GameState -> String -> Int -> GameState
adjustParams gs param delta = case param of
    "rows" -> gs { numRows = min 20 $ max 10 (numRows gs + delta) }
    "cols" -> gs { numCols = min 20 $ max 10 (numCols gs + delta) }
    "mines" -> gs { numMines = min 20 $ max 10 (numMines gs + delta) }
    _ -> gs