module Sprite where

import Rendering

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Graphics.GLUtil.Camera2D
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

  activeTexture $= TextureUnit 2
  textureBinding Texture2D $= Just lur
      
  return (prog, vad, quad)

spriteExampleRender window (prog, vad, quad) = do
  drawAt (prog, vad, quad) window 0 0 0
  drawAt (prog, vad, quad) window 1 2 0
  drawAt (prog, vad, quad) window 1 4 0
  drawAt (prog, vad, quad) window 0 4 2
  drawAt (prog, vad, quad) window 2 (-4) 2

camera winW winH = Linear.ortho (-w) (w) (-h) (h) 0 1
         where w = (winW / spriteSize)::GLfloat
               h = (winH / spriteSize)::GLfloat
               spriteSize = 64

--cam :: Camera GLfloat
--cam = roll 45 $ camera2D
-- (m33_to_m44 $ camMatrix cam)

drawAt (prog, vad, quad) window texUnit x y = do
  bindBuffer ArrayBuffer $= Just quad
  enableAttrib prog "coord2d"
  setAttrib prog "coord2d" ToFloat vad
  setUniform prog "m_transform" (move x y)
  setUniform prog "m_scale" (scaling 1.0 1.0)
  (winW, winH) <- GLFW.getWindowSize window
  setUniform prog "m_cam" (camera (fromIntegral winW) (fromIntegral winH))
  setUniform prog "tex" $ TextureUnit texUnit
  drawArrays TriangleStrip 0 6

