module Motor ( run
             , def
             , MotorSettings(..) )
where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (unless)
import Rendering

type RenderFn = IO ()

data MotorSettings = MotorSettings { windowTitle :: String
                                   , renderFn    :: RenderFn }

def :: MotorSettings
def = MotorSettings {
  windowTitle = "Motor",
  renderFn = return ()
}

run :: MotorSettings -> IO ()
run motorSettings = do
  GLFW.setErrorCallback (Just errorCallback)
  ok <- GLFW.init
  if ok then makeWindow motorSettings else exitFailure

makeWindow :: MotorSettings -> IO ()
makeWindow motorSettings = do
  maybeWindow <- GLFW.createWindow 600 400 (windowTitle motorSettings) Nothing Nothing
  case maybeWindow of
   Nothing -> GLFW.terminate >> exitFailure
   Just window -> startLoop window motorSettings

startLoop :: GLFW.Window -> MotorSettings -> IO ()
startLoop window motorSettings = do
  GLFW.makeContextCurrent (Just window)
  GL.clearColor GL.$= GL.Color4 1 1 0.9 1
  let loop = do
        close <- GLFW.windowShouldClose window
        unless close $ do
          GL.clear [GL.ColorBuffer]
          renderFn motorSettings
          GLFW.swapBuffers window
          GLFW.pollEvents
          loop
  loop -- start the loop
  GLFW.destroyWindow window
  GLFW.terminate
  exitSuccess

errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr
