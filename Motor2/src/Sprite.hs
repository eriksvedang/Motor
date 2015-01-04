module Sprite where

import Rendering

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Storable (sizeOf)
import Linear
import Linear.Matrix
import System.FilePath ((</>))
import Control.Applicative

spriteQuad :: [GLfloat]
spriteQuad  = [-1.0, -1.0,
               -1.0,  1.0,
                1.0,  1.0,
                1.0, -1.0,
               -1.0, -1.0]

loadTex :: FilePath -> IO TextureObject
loadTex f = do t <- either error id <$> readTexture f
               --textureFilter Texture2D $= ((Linear', Nothing), Linear')
               textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
               texture2DWrap $= (Repeated, ClampToEdge)
               return t

spriteExampleSetup = do
  
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  prog <- simpleShaderProgram ("resources" </> "sprite.v.glsl") ("resources" </> "sprite.f.glsl")
  currentProgram $= Just (program prog)
  quad <- makeBuffer ArrayBuffer spriteQuad
  let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
      vad = VertexArrayDescriptor 2 Float stride offset0

  rur <- loadTex ("resources" </> "Rur.png")
  jur <- loadTex ("resources" </> "Jur.png")
  lur <- loadTex ("resources" </> "Lur.png")

  texture Texture2D $= Enabled
  
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just rur

  activeTexture $= TextureUnit 1
  textureBinding Texture2D $= Just jur
      
  return (prog, vad, quad)

spriteExampleRender (prog, vad, quad) = do
  drawAt (prog, vad, quad) 0 (-0.75) 0.3
  drawAt (prog, vad, quad) 1 0.9 (-0.2)

drawAt (prog, vad, quad) n x y = do
  bindBuffer ArrayBuffer $= Just quad
  enableAttrib prog "coord2d"
  setAttrib prog "coord2d" ToFloat vad
  setUniform prog "m_transform" (move x y)
  setUniform prog "m_scale" (scaling 0.25)
  setUniform prog "tex" $ TextureUnit n
  drawArrays TriangleStrip 0 6

scaling :: GLfloat -> M44 GLfloat
scaling s =
  V4 (V4 s 0 0 0)
     (V4 0 s 0 0)
     (V4 0 0 s 0)
     (V4 0 0 0 1)
