module Rendering where

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Storable (sizeOf)





-- prepare :: IO ()
-- prepare = do
--   let (width, height) = (600, 400) -- GLFW.getFramebufferSize win
--   let ratio = fromIntegral width / fromIntegral height
--   viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
--   matrixMode $= Projection
--   loadIdentity
--   ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
--   matrixMode $= Modelview 0
--   loadIdentity

-- line (x1,y1) (x2,y2) =
--     renderPrimitive Lines $ do
--         vertex (Vertex3 (realToFrac x1) (realToFrac y1) 0 :: Vertex3 GLdouble)
--         vertex (Vertex3 (realToFrac x2) (realToFrac y2) 0 :: Vertex3 GLdouble)
        
