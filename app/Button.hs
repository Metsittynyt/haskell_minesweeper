module Button
  ( drawButtons
  , handleButtonEvent
  , initButtons
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GameState (GameState(..), GameStatus(..), initialGameState)
import System.Exit (exitSuccess)

-- Define a button
data Button = Button {
    buttonText   :: String,
    buttonPos    :: Point,
    buttonSize   :: (Float, Float),
    buttonAction :: GameState -> IO GameState
}

-- Initialize buttons with their properties and actions
initButtons :: [Button]
initButtons = [
    Button "New Game" (-110, 220) (100, 40) resetGame,
    Button "Pause" (0, 220) (100, 40) togglePause,
    Button "Exit" (110, 220) (100, 40) exitGame
    ]

-- Draw all buttons
drawButtons :: GameState -> Picture
drawButtons gameState =
    pictures $ map (drawButton gameState) initButtons

-- Draw a single button
drawButton :: GameState -> Button -> Picture
drawButton gameState (Button btnText (px, py) (width, height) _) =
    translate px py $
    color (buttonColor gameState btnText) $
    pictures [
      rectangleSolid width height,
      translate (-width / 2.5) (-height / 8) $ color black $ scale 0.125 0.125 $ text btnText
    ]

-- Determine the button color based on game state
buttonColor :: GameState -> String -> Color
buttonColor gameState text
  | gameStatus gameState == Paused && text == "Pause" = greyN 0.5
  | otherwise = orange

-- Handle events for all buttons, using foldr to apply actions
handleButtonEvent :: Event -> GameState -> IO GameState
handleButtonEvent event gameState = case event of
    EventKey (MouseButton LeftButton) Down _ mousePos ->
        foldr (handleEvent mousePos) (return gameState) initButtons
    _ -> return gameState

-- Check if the mouse click is within any button's bounds and apply its action
handleEvent :: Point -> Button -> IO GameState -> IO GameState
handleEvent (mx, my) (Button _ (px, py) (width, height) action) accGs =
    if mx >= (px - width / 2) && mx <= (px + width / 2) && my >= (py - height / 2) && my <= (py + height / 2)
    then action =<< accGs
    else accGs

-- Define button actions
-- Toggle pause
togglePause :: GameState -> IO GameState
togglePause gs = do
    return gs { gameStatus = if gameStatus gs == Paused then Ongoing else Paused }

resetGame :: GameState -> IO GameState
resetGame gs = do
    newGameState <- initialGameState  -- Initialize a new game state
    return newGameState

exitGame :: GameState -> IO GameState
exitGame gs = do
    exitSuccess