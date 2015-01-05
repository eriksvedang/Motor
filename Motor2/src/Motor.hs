module Motor ( run
             , def
             , MotorSettings(..) )
where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (unless, when)
import Rendering

type RenderFn a = GLFW.Window -> a -> IO ()

data MotorSettings a = MotorSettings
                     { windowTitle :: String
                     , renderFn    :: RenderFn a
                     , setupFn     :: IO a
                     }

def :: MotorSettings ()
def = MotorSettings {
  windowTitle = "Motor",
  renderFn = \_ _ -> return (),
  setupFn = return ()
}

run :: MotorSettings a -> IO ()
run motorSettings = do
  GLFW.setErrorCallback (Just errorCallback)
  ok <- GLFW.init
  if ok then makeWindow motorSettings else exitFailure

makeWindow :: MotorSettings a -> IO ()
makeWindow motorSettings = do
  maybeWindow <- GLFW.createWindow 600 400 (windowTitle motorSettings) Nothing Nothing
  case maybeWindow of
   Nothing -> GLFW.terminate >> exitFailure
   Just window -> startLoop window motorSettings

startLoop :: GLFW.Window -> MotorSettings a -> IO ()
startLoop window motorSettings = do
  GLFW.makeContextCurrent (Just window)
  GL.clearColor $= GL.Color4 1 1 0.9 1
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  setupResult <- (setupFn motorSettings)
  let loop frameInfo = do
        close <- GLFW.windowShouldClose window
        unless close $ do
          frameInfo' <- updateFrameInfo frameInfo
          GL.clear [GL.ColorBuffer]
          (renderFn motorSettings) window setupResult
          GLFW.swapBuffers window
          GLFW.pollEvents
          loop frameInfo'
  loop mkFrameInfo -- start the loop
  GLFW.destroyWindow window
  GLFW.terminate
  exitSuccess

data FrameInfo = FrameInfo {
  frameNr :: Int,
  time :: Double,
  dt :: Double,
  fps :: Int
} deriving (Show)

mkFrameInfo = FrameInfo 0 0.0 0.0 0

updateFrameInfo frameInfo = do
  Just t <- GLFW.getTime
  let frameNr' = (frameNr frameInfo) + 1
  let dt' = t - (time frameInfo) 
  let frameInfo' = frameInfo { frameNr = frameNr'
                             , time = t
                             , dt = dt'
                             , fps = round (1.0 / dt') }
  when ((frameNr' `mod` 30) == 0) (putStrLn ("FPS: " ++ (show (fps frameInfo'))))
  return frameInfo'

errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr
