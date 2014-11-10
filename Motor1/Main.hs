{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

module Main where

import Engine;
import Draw;

main :: IO ()
main = runEngine (WindowSettings "MOTOR" (500, 500) True) initState render update

type GameState = Double

initState :: GameState
initState = 0.0

render :: GameState -> IO ()
render state = line (0, 0) (cos state, 0.5)

update :: Double -> Double -> GameState -> GameState
update knobs dt state = state + dt * knobs
