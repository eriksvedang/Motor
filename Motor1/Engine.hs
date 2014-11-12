module Engine(runEngine, EngineSettings(..)) where

import Control.Monad (unless, when)
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
import Data.IORef
import Control.Concurrent (forkIO)
import Knobs
import Scene

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
    _knobsFile :: String,
    _backgroundColor :: Color4 GLclampf
} deriving (Eq, Show)

runEngine :: Read k => EngineSettings -> k -> g -> SceneManager g k -> IO ()
runEngine engineSettings defaultKnobs initialGameState sceneManager = do

    knobs <- newIORef defaultKnobs
    let knobsFile = _knobsFile engineSettings
    readKnobsSettings knobsFile knobs
    _ <- forkIO (startWatching knobs knobsFile)

    G.setErrorCallback (Just errorCallback)
    successfulInit <- G.init
    if successfulInit then do
        let (width,height) = _size engineSettings
            title = _title engineSettings
            quitWithEscape = _quitWithEscape engineSettings
        mw <- G.createWindow width height title Nothing Nothing
        case mw of
            Nothing -> G.terminate >> exitFailure
            Just win -> do G.makeContextCurrent mw
                           G.setKeyCallback win (Just (keyCallback quitWithEscape))
                           clearColor $= _backgroundColor engineSettings
                           Just t <- G.getTime
                           mainLoop win initialGameState sceneManager knobs t
                           G.destroyWindow win
                           G.terminate
                           exitSuccess
    else exitFailure
          
mainLoop :: Read k => G.Window -> g -> SceneManager g k -> IORef k -> Double -> IO ()
mainLoop window state sceneManager knobs lastT = do
    close <- G.windowShouldClose window
    unless close $ do
        -- Update
        knobState <- readIORef knobs
        Just t <- G.getTime
        let dt = t - lastT
            lastT' = t
            newGameState = updateSceneManager sceneManager state knobState dt
        -- Render
        (width, height) <- G.getFramebufferSize window
        let ratio = fromIntegral width / fromIntegral height
        viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
        clear [ColorBuffer]
        matrixMode $= Projection
        loadIdentity
        ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
        matrixMode $= Modelview 0
        loadIdentity
        --putStrLn $ "dt: " ++ show dt
        renderSceneManager sceneManager state knobState
        G.swapBuffers window
        G.pollEvents
        mainLoop window newGameState sceneManager knobs lastT'
