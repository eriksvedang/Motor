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
  texture2DWrap $= (Repeated, ClampToEdge)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  --textureFilter Texture2D $= ((Linear', Nothing), Linear')
  return tex

data SpriteStore a = SpriteStore {
  prog :: ShaderProgram,
  vad :: VertexArrayDescriptor a,
  quad :: BufferObject,
  textureNamesToUnits :: [(String, CUInt)]
}

assignTextureToUnit :: TextureObject -> CUInt -> IO ()
assignTextureToUnit texture unitNr = do
  activeTexture $= TextureUnit unitNr
  textureBinding Texture2D $= Just texture

getTextureUnit :: SpriteStore a -> String -> TextureUnit
getTextureUnit store textureName =
  case lookup textureName (textureNamesToUnits store) of
   Nothing -> error $ "Failed to find texture with name " ++ textureName ++ " in SpriteStore"
   Just n -> TextureUnit n

mkSpriteStore :: [String] -> IO (SpriteStore a)
mkSpriteStore textureNames = do
  prog <- simpleShaderProgram ("resources" </> "sprite.v.glsl") ("resources" </> "sprite.f.glsl")
  currentProgram $= Just (program prog)
  quad <- makeBuffer ArrayBuffer spriteQuad
  let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
      vad = VertexArrayDescriptor 2 Float stride offset0
      store = SpriteStore {
        prog = prog,
        vad = vad,
        quad = quad,
        textureNamesToUnits = zipWith (,) textureNames [0..]
      }

  textures <- mapM loadTex textureNames
  zipWithM_ assignTextureToUnit textures [0..]

  return store

camera winW winH = Linear.ortho (-w) (w) (-h) (h) 0 1
         where w = (winW / spriteSize)::GLfloat
               h = (winH / spriteSize)::GLfloat
               spriteSize = 64

drawAt store window textureName x y = do
  bindBuffer ArrayBuffer $= Just (quad store)
  enableAttrib (prog store) "coord2d"
  setAttrib (prog store) "coord2d" ToFloat (vad store)
  setUniform (prog store) "m_transform" (move x y)
  setUniform (prog store) "m_scale" (scaling 1.0 1.0)
  (winW, winH) <- GLFW.getWindowSize window
  setUniform (prog store) "m_cam" (camera (fromIntegral winW) (fromIntegral winH))
  setUniform (prog store) "tex" $ (getTextureUnit store textureName)
  drawArrays TriangleStrip 0 6

spriteExampleSetup = do
  store <- mkSpriteStore ["Rur.png", "Jur.png", "Lur.png"]
  return store

spriteExampleRender window store = do
  drawAt store window "Rur.png" 0 0
  drawAt store window "Lur.png" 2 0
  drawAt store window "Jur.png" 4 0

