{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

module Engine(runEngine, WindowSettings(..)) where

import Control.Monad (unless, when, forever)
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
import System.FSNotify
import Control.Concurrent (threadDelay, forkIO)

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

wasModified :: ActionPredicate
wasModified (Modified _ _) = True
wasModified _ = False

startWatching :: IO ()
startWatching =
    withManager $ \mgr -> do
        _ <- watchDir mgr "." wasModified print
        forever $ threadDelay 5

runEngine :: WindowSettings -> a -> (a -> IO ()) -> (Double -> a -> a) -> IO ()
runEngine settings initialState renderFunction updateFunction = do

    _ <- forkIO startWatching

    G.setErrorCallback (Just errorCallback)
    successfulInit <- G.init
    if successfulInit then do
        let (width,height) = _size settings
            title = _title settings
            quitWithEscape = _quitWithEscape settings
        mw <- G.createWindow width height title Nothing Nothing
        case mw of
            Nothing -> G.terminate >> exitFailure
            Just win -> do G.makeContextCurrent mw
                           G.setKeyCallback win (Just (keyCallback quitWithEscape))
                           Just t <- G.getTime
                           mainLoop win initialState renderFunction updateFunction t
                           G.destroyWindow win
                           G.terminate
                           exitSuccess
    else exitFailure
          
mainLoop :: G.Window -> a -> (a -> IO ()) -> (Double -> a -> a) -> Double -> IO ()
mainLoop window state renderFunction updateFunction lastT = do
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
        let dt = t - lastT
            lastT' = t
            state' = updateFunction dt state
        --putStrLn $ "dt: " ++ show dt
        renderFunction state        
        G.swapBuffers window
        G.pollEvents
        mainLoop window state' renderFunction updateFunction lastT'
