module Main where

import Engine
import Draw
import Data.Maybe
import Scene

main :: IO ()
main = runEngine settings ([]::KnobType) initState sceneManager
    where settings = EngineSettings "MOTOR" (500, 500) True "knobs.txt" bgColor
          bgColor = rgbaColor 0.2 0.2 0.2 1.0
          sceneManager = SceneManager [("a", sceneA), ("b", sceneB)] "b"

type KnobType = [(String, Double)]

data GameState = GameState {
    t :: Double,
    stars :: [Star]
}

data Star = Star

sceneA :: Scene GameState
sceneA = Scene (\_ -> line (0,0) (0.3,0.2))
               (\g -> g)

sceneB :: Scene GameState
sceneB = Scene (\g -> line (0, sin (t g)) (negate 0.5, 0.0)) updateSceneB

updateSceneB gameState = gameState { t = t gameState + speed * 0.03 }
    where speed = 5

-- updateSceneB knobs gameState dt = gameState { t = t g + speed * dt }
--     where speed = fromJust $ lookup "speed" knobs

initState :: GameState
initState = GameState 0.0 []
