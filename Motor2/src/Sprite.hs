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

data SpriteStore a = SpriteStore {
  prog :: ShaderProgram,
  vad :: VertexArrayDescriptor a,
  quad :: BufferObject 
}

mkSpriteStore = do
  prog <- simpleShaderProgram ("resources" </> "sprite.v.glsl") ("resources" </> "sprite.f.glsl")
  currentProgram $= Just (program prog)
  quad <- makeBuffer ArrayBuffer spriteQuad
  let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
      vad = VertexArrayDescriptor 2 Float stride offset0
      store = SpriteStore {
        prog = prog,
        vad = vad,
        quad = quad
      }
  return store

spriteExampleSetup = do
  
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

  store <- mkSpriteStore
    
  return store

spriteExampleRender window store = do
  drawAt store window 0 0 0
  drawAt store window 1 2 0
  drawAt store window 1 4 0
  drawAt store window 0 4 2
  drawAt store window 2 (-4) 2

camera winW winH = Linear.ortho (-w) (w) (-h) (h) 0 1
         where w = (winW / spriteSize)::GLfloat
               h = (winH / spriteSize)::GLfloat
               spriteSize = 64

--cam :: Camera GLfloat
--cam = roll 45 $ camera2D
-- (m33_to_m44 $ camMatrix cam)

drawAt store window texUnit x y = do
  bindBuffer ArrayBuffer $= Just (quad store)
  enableAttrib (prog store) "coord2d"
  setAttrib (prog store) "coord2d" ToFloat (vad store)
  setUniform (prog store) "m_transform" (move x y)
  setUniform (prog store) "m_scale" (scaling 1.0 1.0)
  (winW, winH) <- GLFW.getWindowSize window
  setUniform (prog store) "m_cam" (camera (fromIntegral winW) (fromIntegral winH))
  setUniform (prog store) "tex" $ TextureUnit texUnit
  drawArrays TriangleStrip 0 6

