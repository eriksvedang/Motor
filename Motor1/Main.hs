module Main where

import Engine
import Draw
import Knobs
import Control.Monad.State
-- import qualified Graphics.UI.GLFW as G

main :: IO ()
main = do 
    knobsRef <- watchAsync [] "knobs.txt"
    runEngine def (GameState 0.0 knobsRef) update render

type KnobsRef = IORef [(String, Double)]

data GameState = GameState {
    t :: Double,
    r :: KnobsRef
}

update :: UpdateFn GameState
update dt = do s <- get
               let knobsRef = r s
               speed <- liftIO $ readSetting "speed" knobsRef
               modify (passTime $ dt * speed)
               when (t s > 1.0) $ put (GameState 0.0 knobsRef)

passTime :: Double -> GameState -> GameState
passTime dt s = s { t = t s + dt }

render :: RenderFn GameState
render = do s <- ask
            liftIO $ line (0,0) (t s, 0)
