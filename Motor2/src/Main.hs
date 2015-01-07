module Main where

import Motor (run, def, MotorSettings(..))
import Sprite
--import Rendering

-- import Graphics.Rendering.OpenGL
-- import Graphics.GLUtil
-- import Foreign.Storable (sizeOf)
-- import System.FilePath ((</>))
-- import Linear

main :: IO ()
main = run $ def { setupFn = spriteExampleSetup
                 , renderFn = spriteExampleRender
                 , windowTitle = "M O T O R 2"
                 }

