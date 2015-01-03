module Motor ( run ) where

import qualified Graphics.UI.GLFW as GLFW
import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitFailure)
import Control.Monad

run :: IO ()
run = do
  GLFW.setErrorCallback (Just errorCallback)
  ok <- GLFW.init
  if ok then makeWindow else exitFailure

makeWindow :: IO ()
makeWindow = do
  maybeWindow <- GLFW.createWindow 600 400 "λ" Nothing Nothing
  case maybeWindow of
   Nothing -> GLFW.terminate >> exitFailure
   Just window -> startLoop window

startLoop :: GLFW.Window -> IO ()
startLoop window = do
  GLFW.makeContextCurrent (Just window)
  let loop = do
        close <- GLFW.windowShouldClose window
        unless close $ do
          GLFW.swapBuffers window
          GLFW.pollEvents
          loop
  loop -- start the loop
  GLFW.destroyWindow window
  GLFW.terminate
  exitSuccess

errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr
