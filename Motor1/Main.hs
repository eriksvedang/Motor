module Main where

import Engine;
import Draw;
import Data.Maybe

main :: IO ()
main = runEngine settings [] initState render update
    where settings = WindowSettings "MOTOR" (500, 500) True "knobs.txt" darkGray
          darkGray = rgbaColor 0.2 0.2 0.2 1.0

type GameState = Double
type KnobType = [(String, Double)]

initState :: GameState
initState = 0.0

render :: KnobType -> GameState -> IO ()
render knobs state = line (0, 0) (cos state, w)
    where w = fromJust $ lookup "width" knobs

update :: KnobType -> GameState -> Double -> GameState
update knobs state dt = state + dt * speed
    where speed = fromJust $ lookup "speed" knobs
