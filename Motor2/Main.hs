module Main where

import Motor (run, def, MotorSettings(..))

main :: IO ()
main = run $ def {renderFn = render}

render :: IO ()
render = putStrLn "hej"

