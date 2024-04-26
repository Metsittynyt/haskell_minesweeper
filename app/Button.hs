module Button
  ( drawButtons
  , handleButtonEvent
  , Button(..)  -- Exporting the Button data type
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GameState (GameState(..))

-- Define a button
data Button = Button {
    buttonText   :: String,
    buttonPos    :: Point,
    buttonSize   :: (Float, Float),
    buttonColor  :: GameState -> Color,  -- Function to determine color dynamically
    buttonAction :: GameState -> IO GameState
}

-- Draw all buttons
drawButtons :: GameState -> [Button] -> Picture
drawButtons gameState buttons =
    pictures $ map (drawButton gameState) buttons

-- Draw a single button
drawButton :: GameState -> Button -> Picture
drawButton gameState (Button btnText (px, py) (width, height) getColor _) =
    translate px py $
    color (getColor gameState) $
    pictures [
      rectangleSolid width height,
      translate (- (width / 2.5)) (- (height / 8)) $ color black $ scale 0.125 0.125 $ text btnText
    ]

-- Handle events for all buttons, using foldr to apply actions
handleButtonEvent :: Event -> GameState -> [Button] -> IO GameState
handleButtonEvent event gameState buttons = case event of
    EventKey (MouseButton LeftButton) Down _ mousePos ->
        foldr (handleEvent mousePos) (return gameState) buttons
    _ -> return gameState

-- Check if the mouse click is within any button's bounds and apply its action
handleEvent :: Point -> Button -> IO GameState -> IO GameState
handleEvent (mx, my) (Button _ (px, py) (width, height) _ action) accGs =
    if mx >= px - width / 2 && mx <= px + width / 2 && my >= py - height / 2 && my <= py + height / 2
    then action =<< accGs
    else accGs
