module Main where

import Engine
import Draw
import Data.Maybe
import Scene

main :: IO ()
main = runEngine settings defaultKnobs initState sceneManager
    where settings = EngineSettings "MOTOR" (500, 500) True "knobs.txt" bgColor
          sceneManager = SceneManager [("a", sceneA), ("b", sceneB)] "b"
          bgColor = rgbaColor 0.2 0.2 0.2 1.0
          defaultKnobs = []::KnobType

type KnobType = [(String, Double)]

data GameState = GameState {
    t :: Double
}

sceneA :: Scene GameState KnobType
sceneA = Scene (\_ _ -> line (0,0) (0.3,0.2))
               (\g _ _ -> g)

sceneB :: Scene GameState KnobType
sceneB = Scene renderSceneB
               updateSceneB

renderSceneB :: GameState -> KnobType -> IO ()
renderSceneB gameState _ = line (0, sin (t gameState)) (negate 0.5, 0.0)

updateSceneB :: GameState -> KnobType -> Double -> GameState
updateSceneB gameState knobs dt = gameState { t = t gameState + speed * dt }
    where speed = fromJust $ lookup "speed" knobs

initState :: GameState
initState = GameState 0.0
