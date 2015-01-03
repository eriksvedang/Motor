module Main where

import Motor (run, def, MotorSettings(..))
import Rendering

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Foreign.Storable (sizeOf)
import System.FilePath ((</>))

main :: IO ()
main = do
  run $ def {renderFn = render}

--render :: Program -> IO ()
render = do
  vs <- loadShader VertexShader $ "resources" </> "shader.vert"
  fs <- loadShader FragmentShader $ "resources" </> "shader.frag"
  prog <- linkShaderProgram [vs,fs]
  currentProgram $= Just prog
  vb <- makeBuffer ArrayBuffer vertexBufferData
  posn <- get (attribLocation prog "coord2d")
  let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
      vad = VertexArrayDescriptor 2 Float stride offset0
  bindBuffer ArrayBuffer   $= Just vb
  vertexAttribPointer posn $= (ToFloat, vad)
  vertexAttribArray posn   $= Enabled
  drawArrays Triangles 0 3

