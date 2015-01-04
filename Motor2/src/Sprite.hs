module Sprite where

import Rendering

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Storable (sizeOf)
import Linear
import System.FilePath ((</>))
import Control.Applicative

spriteQuad :: [GLfloat]
spriteQuad  = [-1, -1,
               -1,  1,
                1,  1,
                1, -1,
               -1, -1]

loadTex :: FilePath -> IO TextureObject
loadTex f = do t <- either error id <$> readTexture f
               textureFilter Texture2D $= ((Linear', Nothing), Linear')
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

  putStrLn $ "rur: " ++ show rur
  putStrLn $ "jur: " ++ show jur
  putStrLn $ "lur: " ++ show lur
  
  --textureFilter Texture2D $= ((Linear', Nothing), Linear')
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

  --textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  --textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)

  texture Texture2D $= Enabled
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just rur

  activeTexture $= TextureUnit 1
  textureBinding Texture2D $= Just jur
      
  return (prog, vad, quad, rur, jur)

spriteExampleRender (prog, vad, quad, rur, jur) = do
  --drawAt 0 0
  drawAt (prog, vad, quad, rur, jur) 0.2 0

drawAt (prog, vad, quad, rur, jur) x y = do
  bindBuffer ArrayBuffer $= Just quad
  enableAttrib prog "coord2d"
  setAttrib prog "coord2d" ToFloat vad
  setUniform prog "m_transform" (move x y)
  setUniform prog "tex" $ TextureUnit 1
  drawArrays TriangleStrip 0 6
