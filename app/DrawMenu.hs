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
       drawButtons gameState menuButtons,
       drawParameters gameState 
    ]

-- Define buttons for the menu screen
menuButtons :: [Button]
menuButtons = [
    Button "Play" (-10, -50) (100, 40) (const orange) startGame,
    Button "Exit" (-10, -100) (100, 40) (const red) exitGame
    ]

-- Function to start a new game
startGame :: GameState -> IO GameState
startGame gs = do
    putStrLn "Starting a new game..."
    newGameState <- initializeGame
    return gs { gameScreen = Game}

-- Function to exit the game
exitGame :: GameState -> IO GameState
exitGame gs = do
    putStrLn "Exiting game..."
    exitSuccess

-- Draw changeable parameters
drawParameters :: GameState -> Picture
drawParameters gameState =
    pictures
        [ translate (-90) 140 $ scale 0.125 0.125 $ color black $ text "Number of mines: "
        , translate (-90) 80 $ scale 0.125 0.125 $ color black $ text "Number of rows: "
        , translate (-90) 20 $ scale 0.125 0.125 $ color black $ text "Number of columns: "
        ]
