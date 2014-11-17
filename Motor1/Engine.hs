module Engine(runEngine,
              def,
              EngineSettings(..),
              UpdateFn,
              RenderFn,
              modify,
              ask,
              liftIO) where

import Colors
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
import Control.Monad
import Control.Monad.State (execStateT, StateT, modify)
import Control.Monad.Reader
--import Control.Monad.IO.Class
-- import Control.Monad.Identity
-- import Data.IORef
-- import Control.Concurrent (forkIO)
-- import Knobs
-- import Scene
-- import Event

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback _ = hPutStrLn stderr

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback :: Bool -> G.KeyCallback
keyCallback quitWithEscape window key _ keyState _ = 
    when (quitWithEscape && key == G.Key'Escape && keyState == G.KeyState'Pressed) $ G.setWindowShouldClose window True

data EngineSettings = EngineSettings {
    _title :: String,
    _size :: (Int, Int),
    _quitWithEscape :: Bool,
    _backgroundColor :: Color4 GLclampf
} deriving (Eq, Show)

type UpdateFn s = Double -> StateT s IO ()
type RenderFn s = ReaderT s IO ()

runEngine :: EngineSettings -> s -> UpdateFn s -> RenderFn s -> IO ()
runEngine engineSettings initialGameState updateFn renderFn = do

    G.setErrorCallback (Just errorCallback)
    successfulInit <- G.init

    if successfulInit then do

        let (initWidth, initHeight) = _size engineSettings
            title = _title engineSettings
            quitWithEscape = _quitWithEscape engineSettings

        maybeWindow <- G.createWindow initWidth initHeight title Nothing Nothing
        case maybeWindow of
            Nothing -> G.terminate >> exitFailure
            Just win -> do G.makeContextCurrent maybeWindow
                           G.setKeyCallback win $ Just (keyCallback quitWithEscape)
                           clearColor $= _backgroundColor engineSettings
                           
                           let mainLoop state lastT = do
                                close <- G.windowShouldClose win
                                unless close $ do
                                    -- Update
                                    Just t <- G.getTime
                                    let dt = t - lastT
                                    state' <- execStateT (updateFn dt) state

                                    -- Render
                                    (width, height) <- G.getFramebufferSize win
                                    let ratio = fromIntegral width / fromIntegral height
                                    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
                                    clear [ColorBuffer]
                                    matrixMode $= Projection
                                    loadIdentity
                                    ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
                                    matrixMode $= Modelview 0
                                    loadIdentity
                                    runReaderT renderFn state
                                    G.swapBuffers win
                                    G.pollEvents
                                    mainLoop state' t

                           Just initT <- G.getTime
                           mainLoop initialGameState initT
                           G.destroyWindow win
                           G.terminate
                           exitSuccess
    else exitFailure

def :: EngineSettings
def = EngineSettings {
        _title = "MOTOR",
        _size = (500, 500),
        _quitWithEscape = True,
        _backgroundColor = rgbaColor 0.2 0.2 0.2 1.0
      }