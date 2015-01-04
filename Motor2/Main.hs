module Main where

import Motor (run, def, MotorSettings(..))
import Rendering

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Foreign.Storable (sizeOf)
import System.FilePath ((</>))
import Linear

main :: IO ()
main = run $ def { renderFn = render
                 , setupFn = setup
                 }

vertexBufferData, vertexBufferData2 :: [GLfloat]
vertexBufferData  = [-1, 0, 0, 1, 1, 0]
vertexBufferData2 = [-0.5, 0, 0, -1, 1, 0]

setup = do
  prog <- simpleShaderProgram ("resources" </> "shader.vert") ("resources" </> "shader.frag")
  currentProgram $= Just (program prog)
  a <- makeBuffer ArrayBuffer vertexBufferData
  b <- makeBuffer ArrayBuffer vertexBufferData2
  let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
      vad = VertexArrayDescriptor 2 Float stride offset0
  return (prog, vad, a, b)

render (prog, vad, a, b) = do
  bindBuffer ArrayBuffer $= Just a
  enableAttrib prog "coord2d"
  setAttrib prog "coord2d" ToFloat vad
  setUniform prog "col" (V3 (0::GLfloat) 1 0)
  drawArrays Triangles 0 3

  bindBuffer ArrayBuffer $= Just b
  enableAttrib prog "coord2d"
  setAttrib prog "coord2d" ToFloat vad
  setUniform prog "col" (V3 (1::GLfloat) 1 0)
  drawArrays Triangles 0 3

