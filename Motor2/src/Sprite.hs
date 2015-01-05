module Sprite where

import Rendering

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Graphics.GLUtil.Camera2D
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Storable (sizeOf)
import Foreign.C.Types (CUInt)
import Linear
import Linear.Matrix
import System.FilePath ((</>))
import Control.Applicative
import Control.Monad (zipWithM_)

spriteQuad :: [GLfloat]
spriteQuad  = [-1.0, -1.0,
               -1.0,  1.0,
                1.0,  1.0,
                1.0, -1.0,
               -1.0, -1.0]

loadTex :: String -> IO TextureObject
loadTex name = do
  tex <- either error id <$> readTexture ("resources" </> name)
  --textureFilter Texture2D $= ((Linear', Nothing), Linear')
  texture2DWrap $= (Repeated, ClampToEdge)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  --putStrLn $ "Bound " ++ name ++ ", " ++ show tex ++ " to texture unit " ++ show textureUnitNr
  return tex

data SpriteStore a = SpriteStore {
  prog :: ShaderProgram,
  vad :: VertexArrayDescriptor a,
  quad :: BufferObject 
}

assignTextureToNr texture nr = do
  activeTexture $= TextureUnit nr
  textureBinding Texture2D $= Just texture

mkSpriteStore spriteNames = do
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

  textures <- mapM loadTex spriteNames
  zipWithM_ assignTextureToNr textures [0..]

  return store

spriteExampleSetup = do
  store <- mkSpriteStore ["Rur.png", "Jur.png", "Lur.png"]
  return store

spriteExampleRender window store = do
  drawAt store window 0 0 0
  drawAt store window 1 2 0
  drawAt store window 2 4 0

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

