module Engine(runEngine, WindowSettings(..)) where

import Control.Monad (unless, when)
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
import Data.IORef
import Control.Concurrent (forkIO)
import Knobs

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback _ = hPutStrLn stderr

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback :: Bool -> G.KeyCallback
keyCallback quitWithEscape window key _ keyState _ = 
    when (quitWithEscape && key == G.Key'Escape && keyState == G.KeyState'Pressed) $ G.setWindowShouldClose window True

data WindowSettings = WindowSettings {
    _title :: String,
    _size :: (Int, Int),
    _quitWithEscape :: Bool,
    _knobsFile :: String
} deriving (Eq, Show)

runEngine :: WindowSettings -> a -> (a -> IO ()) -> (Double -> Double -> a -> a) -> IO ()
runEngine windowSettings initialState renderFunction updateFunction = do

    knobs <- newIORef (0.0 :: Double)

    let knobsFile = _knobsFile windowSettings
    readKnobsSettings knobsFile knobs

    _ <- forkIO (startWatching knobs knobsFile)

    G.setErrorCallback (Just errorCallback)
    successfulInit <- G.init
    if successfulInit then do
        let (width,height) = _size windowSettings
            title = _title windowSettings
            quitWithEscape = _quitWithEscape windowSettings
        mw <- G.createWindow width height title Nothing Nothing
        case mw of
            Nothing -> G.terminate >> exitFailure
            Just win -> do G.makeContextCurrent mw
                           G.setKeyCallback win (Just (keyCallback quitWithEscape))
                           Just t <- G.getTime
                           mainLoop win initialState renderFunction updateFunction knobs t
                           G.destroyWindow win
                           G.terminate
                           exitSuccess
    else exitFailure
          
mainLoop :: G.Window -> a -> (a -> IO ()) -> (Double -> Double -> a -> a) -> IORef Double -> Double -> IO ()
mainLoop window state renderFunction updateFunction knobs lastT = do
    close <- G.windowShouldClose window
    unless close $ do
        (width, height) <- G.getFramebufferSize window
        let ratio = fromIntegral width / fromIntegral height
        viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
        clear [ColorBuffer]
        matrixMode $= Projection
        loadIdentity
        ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
        matrixMode $= Modelview 0
        loadIdentity
        Just t <- G.getTime
        knobState <- readIORef knobs
        let dt = t - lastT
            lastT' = t
            state' = updateFunction knobState dt state
        --putStrLn $ "dt: " ++ show dt
        renderFunction state        
        G.swapBuffers window
        G.pollEvents
        mainLoop window state' renderFunction updateFunction knobs lastT'
