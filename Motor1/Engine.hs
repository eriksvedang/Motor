{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

module Engine(runEngine, WindowSettings(..)) where

import Control.Monad (unless, when, forever)
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
import System.FSNotify
import Filesystem.Path.CurrentOS
import Control.Concurrent (threadDelay, forkIO)
import Data.IORef

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
    _quitWithEscape :: Bool
} deriving (Eq, Show)

wasItModified :: ActionPredicate
wasItModified (Modified _ _) = True
wasItModified _ = False

readKnobSettings :: String -> IORef Double -> IO ()
readKnobSettings path knobs = do
    handle <- openFile path ReadMode  
    contents <- hGetContents handle  
    let newValue = read contents :: Double
    writeIORef knobs newValue
    hClose handle

onModified :: IORef Double -> Action
onModified knobs (Modified path _) =
    --putStrLn $ show path ++ " was modified"
    when (filename path == "data.txt") $ readKnobSettings (encodeString path) knobs
        
onModified _ _ = return ()

startWatching :: IORef Double -> IO ()
startWatching knobs =
    withManager $ \mgr -> do
        _ <- watchDir mgr "." wasItModified (onModified knobs)
        forever $ threadDelay 5

runEngine :: WindowSettings -> a -> (a -> IO ()) -> (Double -> Double -> a -> a) -> IO ()
runEngine windowSettings initialState renderFunction updateFunction = do

    knobs <- newIORef (0.0 :: Double)
    readKnobSettings "data.txt" knobs

    _ <- forkIO (startWatching knobs)

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
