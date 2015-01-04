module Rendering where

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Storable (sizeOf)
import Linear
import System.FilePath ((</>))

move :: GLfloat -> GLfloat -> M44 GLfloat
move x y = mkTransformation ident (V3 x y 0.0)
       where ident = Quaternion 0.0 (V3 0 0 0)

vertexBufferData, vertexBufferData2 :: [GLfloat]
vertexBufferData  = [-0.1, 0, 0, 0.1, 0.1, 0]
vertexBufferData2 = [-0.5, 0, 0, -1, 1, 0]

shapeExampleSetup = do
  prog <- simpleShaderProgram ("resources" </> "shape.v.glsl") ("resources" </> "shape.f.glsl")
  currentProgram $= Just (program prog)
  a <- makeBuffer ArrayBuffer vertexBufferData
  b <- makeBuffer ArrayBuffer vertexBufferData2
  let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
      vad = VertexArrayDescriptor 2 Float stride offset0
  return (prog, vad, a, b)

shapeExampleRender (prog, vad, a, b) = do
  bindBuffer ArrayBuffer $= Just a
  enableAttrib prog "coord2d"
  setAttrib prog "coord2d" ToFloat vad
  setUniform prog "col" (V3 (0::GLfloat) 1 0)
  setUniform prog "m_transform" (move (-0.2) 0.0)
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
        
