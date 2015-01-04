module Sprite where

import Rendering

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Storable (sizeOf)
import Linear
import System.FilePath ((</>))

vertexBufferData3, vertexBufferData4 :: [GLfloat]
vertexBufferData3  = [-0.1, 0, 0, 0.1, 0.1, 0]
vertexBufferData4 = [-0.5, 0, 0, -1, 1, 0]

spriteExampleSetup = do
  prog <- simpleShaderProgram ("resources" </> "shape.v.glsl") ("resources" </> "shape.f.glsl")
  currentProgram $= Just (program prog)
  a <- makeBuffer ArrayBuffer vertexBufferData3
  b <- makeBuffer ArrayBuffer vertexBufferData4
  let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
      vad = VertexArrayDescriptor 2 Float stride offset0
  return (prog, vad, a, b)

spriteExampleRender (prog, vad, a, b) = do
  bindBuffer ArrayBuffer $= Just a
  enableAttrib prog "coord2d"
  setAttrib prog "coord2d" ToFloat vad
  setUniform prog "col" (V3 (0::GLfloat) 1 0)
  setUniform prog "m_transform" (move (-0.2) 0.0)
  drawArrays Triangles 0 3

