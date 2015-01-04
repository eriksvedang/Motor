module Sprite where

import Rendering

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Storable (sizeOf)
import Linear
import System.FilePath ((</>))

spriteQuad :: [GLfloat]
spriteQuad  = [-1, -1,
               -1,  1,
                1,  1,
                1, -1,
               -1, -1]

spriteExampleSetup = do
  Right rur <- readTexture ("resources" </> "Rur.png")

  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)

  prog <- simpleShaderProgram ("resources" </> "shape.v.glsl") ("resources" </> "shape.f.glsl")
  currentProgram $= Just (program prog)
  a <- makeBuffer ArrayBuffer spriteQuad
  b <- makeBuffer ArrayBuffer spriteQuad
  let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
      vad = VertexArrayDescriptor 2 Float stride offset0
  return (prog, vad, a, b)

spriteExampleRender (prog, vad, a, b) = do
  bindBuffer ArrayBuffer $= Just a
  enableAttrib prog "coord2d"
  setAttrib prog "coord2d" ToFloat vad
  setUniform prog "col" (V3 (0::GLfloat) 1 0)
  setUniform prog "m_transform" (move 0 0)
  drawArrays TriangleStrip 0 6

