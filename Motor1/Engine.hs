module Engine(runEngine,
              def,
              EngineSettings(..),
              UpdateFn,
              RenderFn,
              InputFn,
              modify,
              ask,
              liftIO
              ) where

import Colors
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
import Control.Monad
import Control.Monad.State (execStateT, modify)
import Control.Monad.Reader
import Types
import Data.IORef

errorCallback :: G.ErrorCallback
errorCallback _ = hPutStrLn stderr

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback :: InputFn s -> IORef s -> Bool -> G.KeyCallback
keyCallback inputFn stateRef quitWithEscape window key i keyState modifierKeys = do
    state <- readIORef stateRef
    newState <- execStateT (inputFn key i keyState modifierKeys) state
    writeIORef stateRef newState
    when (quitWithEscape && key == G.Key'Escape && keyState == G.KeyState'Pressed) $ G.setWindowShouldClose window True

data EngineSettings = EngineSettings {
    _title :: String,
    _size :: (Int, Int),
    _quitWithEscape :: Bool,
    _backgroundColor :: Color4 GLclampf
} deriving (Eq, Show)

runEngine :: EngineSettings -> s -> UpdateFn s -> RenderFn s -> InputFn s -> IO ()
runEngine engineSettings initialGameState updateFn renderFn inputFn = do

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

                           gameStateRef <- newIORef initialGameState

                           G.setKeyCallback win $ Just (keyCallback inputFn gameStateRef quitWithEscape)
                           clearColor $= _backgroundColor engineSettings
                           
                           let mainLoop lastT = do
                                state <- readIORef gameStateRef
                                close <- G.windowShouldClose win
                                unless close $ do
                                    -- Update
                                    Just t <- G.getTime
                                    let dt = t - lastT
                                    state' <- execStateT (updateFn dt) state
                                    writeIORef gameStateRef state'

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
                                    stateToRender <- readIORef gameStateRef
                                    runReaderT renderFn stateToRender
                                    G.swapBuffers win
                                    G.pollEvents
                                    mainLoop t

                           Just initT <- G.getTime
                           mainLoop initT
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
