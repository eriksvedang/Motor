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
  Right jur <- readTexture ("resources" </> "Jur.png")

  --textureFilter Texture2D $= ((Linear', Nothing), Linear')
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

  --textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  --textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)

  --activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just rur

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  prog <- simpleShaderProgram ("resources" </> "sprite.v.glsl") ("resources" </> "sprite.f.glsl")
  currentProgram $= Just (program prog)
  quad <- makeBuffer ArrayBuffer spriteQuad
  let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
      vad = VertexArrayDescriptor 2 Float stride offset0
      
  return (prog, vad, quad)

spriteExampleRender (prog, vad, quad) = do
  --drawAt 0 0
  drawAt (prog, vad, quad) 0.2 0

drawAt (prog, vad, quad) x y = do
  bindBuffer ArrayBuffer $= Just quad
  enableAttrib prog "coord2d"
  setAttrib prog "coord2d" ToFloat vad
  setUniform prog "m_transform" (move x y)
  --setUniform prog "texture0" (TextureUnit 0)
  drawArrays TriangleStrip 0 6
