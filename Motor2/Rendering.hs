module Rendering where

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as GLFW
import System.FilePath ((</>))
import Foreign.Storable (sizeOf)

vertexBufferData :: [GLfloat]
vertexBufferData = [-1,  0,
                     0,  1,
                     1,  0]

-- vs <- loadShader VertexShader $ "resources" </> "shader.vert"
-- fs <- loadShader FragmentShader $ "resources" </> "shader.frag"
-- prog <- linkShaderProgram [vs,fs]

drawingTest = do

  prog <- simpleShaderProgram "resources/shader.vert" "resources/shader.frag"
  currentProgram $= Just (program prog)

  let posn = getAttrib prog "coord2d"
      stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
      vad = VertexArrayDescriptor 2 Float stride offset0
      
  vb <- makeBuffer ArrayBuffer vertexBufferData
  
  bindBuffer ArrayBuffer   $= Just vb
  vertexAttribPointer posn $= (ToFloat, vad)
  vertexAttribArray posn   $= Enabled

  drawArrays Triangles 0 3



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
        
