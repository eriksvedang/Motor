module Main where

import Motor (run, def, MotorSettings(..))
import Rendering

main :: IO ()
main = run $ def {renderFn = render}

render :: IO ()
render = drawingTest

