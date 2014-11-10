module Tailspin where

import Engine;
import Draw;

tailspin :: IO ()
tailspin = runEngine (WindowSettings "TAILSPIN" (500, 500) True) initState render update

type State = Double

initState :: State
initState = 0.0

render :: State -> IO ()
render state = mapM_ (rToArm state) [0.0, 0.01 .. 0.9]

rToArm :: Double -> Double -> IO ()
rToArm v r = arm (v * r) r

arm :: Double -> Double -> IO ()
arm v r = line (0.0, 0.0) (cos v * r, sin v * r)

update :: Double -> State -> State
update dt state = state + dt * 2.0
