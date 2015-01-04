module Main where

import Motor (run, def, MotorSettings(..))
import Rendering

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Foreign.Storable (sizeOf)
import System.FilePath ((</>))
import Linear

main :: IO ()
main = run $ def { renderFn = shapeExampleRender
                 , setupFn = shapeExampleSetup
                 }


