module Main where

import Engine
import Draw
import Knobs
import Scene
import Control.Monad.State
-- import qualified Graphics.UI.GLFW as G

main :: IO ()
main = do 
    knobsRef <- watchAsync [] "knobs.txt"
    runEngine def (GameState 0.5 knobsRef sceneMgr) update render

sceneMgr :: SceneManager GameState
sceneMgr = SceneManager [("a", sceneA), ("b", sceneB)] "b"

sceneA :: Scene GameState
sceneA = Scene {
    updateFn = passTime,
    renderFn = return ()
}

sceneB :: Scene GameState
sceneB = Scene {
    updateFn = reverseTime,
    renderFn = return ()
}

type KnobsRef = IORef [(String, Double)]

data GameState = GameState {
    t :: Double,
    _knobsRef :: KnobsRef,
    _sceneMgr :: SceneManager GameState
}

instance Show GameState where
    show s = "GameState, t = " ++ show (t s)

update :: UpdateFn GameState
update dt = do s <- get
               let mgr = _sceneMgr s
               updateSceneManager mgr dt
               s' <- get
               when (t s' > 1.0) $ newScene "b"
               when (t s' < 0.0) $ newScene "a"

newScene :: String -> StateT GameState IO ()
newScene name = do s <- get
                   put $ s { _sceneMgr = setScene (_sceneMgr s) name }

readKnob :: String -> StateT GameState IO Double
readKnob name = do s <- get
                   liftIO $ readSetting name (_knobsRef s)

passTime :: Double -> StateT GameState IO ()
passTime dt = do 
    s <- get 
    speed <- readKnob "speed"
    put s { t = t s + dt * speed }

reverseTime :: Double -> StateT GameState IO ()
reverseTime dt = do 
    s <- get 
    put s { t = t s - dt }

render :: RenderFn GameState
render = do s <- ask
            liftIO $ line (0,0) (t s, 0)
