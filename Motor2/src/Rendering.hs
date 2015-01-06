module Rendering where

import Graphics.Rendering.OpenGL
import Linear

--import Graphics.GLUtil
--import qualified Graphics.UI.GLFW as GLFW
--import Foreign.Storable (sizeOf)
--import System.FilePath ((</>))

move :: GLfloat -> GLfloat -> M44 GLfloat
move x y = mkTransformation ident (V3 x y 0.0)
       where ident = Quaternion 0.0 (V3 0 0 0)

scaling :: GLfloat -> GLfloat -> M44 GLfloat
scaling w h =
  V4 (V4 w 0 0 0)
     (V4 0 h 0 0)
     (V4 0 0 1 0)
     (V4 0 0 0 1)
