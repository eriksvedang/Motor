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
  GL.clearColor GL.$= GL.Color4 1 1 0.9 1
  renderData <- (setupFn motorSettings)
  let loop = do
        close <- GLFW.windowShouldClose window
        unless close $ do
          GL.clear [GL.ColorBuffer]
          (renderFn motorSettings) window renderData
          GLFW.swapBuffers window
          GLFW.pollEvents
          loop
  loop -- start the loop
  GLFW.destroyWindow window
  GLFW.terminate
  exitSuccess

errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr
