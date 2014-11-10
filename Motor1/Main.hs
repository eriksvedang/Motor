module Main where

import Engine;
import Draw;

main :: IO ()
main = tailspin

tailspin :: IO ()
tailspin = runEngine (WindowSettings "MOTOR" (500, 500) True) initState render update

type State = Double

initState :: State
initState = 0.0

render :: State -> IO ()
render state = line (0, 0) (cos state, 0.5)

update :: Double -> State -> State
update dt state = state + dt * 2.0
