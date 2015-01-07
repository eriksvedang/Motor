module Main where

import Motor (run, def, MotorSettings(..))
--import Rendering
import Sprite

-- import Graphics.Rendering.OpenGL
-- import Graphics.GLUtil
-- import Foreign.Storable (sizeOf)
-- import System.FilePath ((</>))
-- import Linear

main = run $ def { setupFn = spriteExampleSetup
                 , renderFn = spriteExampleRender
                 , windowTitle = "M O T O R 2"
                 }
