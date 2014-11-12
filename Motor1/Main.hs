module Main where

import Engine
import Draw
import Data.Maybe
import Scene
import Event

main :: IO ()
main = runEngine settings defaultKnobs initState sceneManager
    where settings = EngineSettings "MOTOR" (500, 500) True "knobs.txt" bgColor
          sceneManager = SceneManager [("a", sceneA), ("b", sceneB)] "a"
          bgColor = rgbaColor 0.2 0.2 0.2 1.0
          defaultKnobs = []::KnobType

type KnobType = [(String, Double)]

data GameState = GameState {
    t :: Double
}

initState :: GameState
initState = GameState 0.0


sceneA :: Scene GameState KnobType
sceneA = Scene renderSceneA
               updateSceneA
renderSceneA :: GameState -> KnobType -> IO ()
renderSceneA gameState _ = line (0, 0) (t gameState / 10.0, 0)

updateSceneA :: GameState -> KnobType -> Double -> (GameState, [Event])
updateSceneA gameState _ dt = 
    (gameState { t = t gameState + dt }, 
     if t gameState < 3.0 then [] else [ChangeScene "b", SetWindowSize (200, 200)])


sceneB :: Scene GameState KnobType
sceneB = Scene renderSceneB
               updateSceneB

renderSceneB :: GameState -> KnobType -> IO ()
renderSceneB gameState _ = line (0, sin (t gameState)) (negate 0.5, 0.0)

updateSceneB :: GameState -> KnobType -> Double -> (GameState, [Event])
updateSceneB gameState knobs dt = 
    (gameState { t = t gameState + speed * dt }, events)
        where speed = fromJust $ lookup "speed" knobs
              events = if t gameState < 7.0 then [] else [Quit]


