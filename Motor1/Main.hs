module Main where

import Engine
import Draw
--import Data.Maybe
import Scene

main :: IO ()
main = runEngine settings [] initState render update
    where settings = EngineSettings "MOTOR" (500, 500) True "knobs.txt" bgColor
          bgColor = rgbaColor 0.2 0.2 0.2 1.0

type KnobType = [(String, Double)]

data GameState = GameState {
    t :: Double,
    stars :: [Star],
    sceneManager :: SceneManager String GameState
}

data Star = Star {
    
}

sceneA :: Scene GameState
sceneA = Scene (\_ -> line (0,0) (0.3,0.2))
               id

sceneB :: Scene GameState
sceneB = Scene (\g -> line (0, sin (t g)) (negate 0.5, 0.0)) (\g -> g { t = t g + 0.1 })

initState :: GameState
initState = GameState 0.0 [] (SceneManager [("a", sceneA), ("b", sceneB)] "b")

render :: KnobType -> GameState -> IO ()
render _ state = renderSceneManager (sceneManager state) state

update :: KnobType -> GameState -> Double -> GameState
update _ state _ = updateSceneManager (sceneManager state) state
