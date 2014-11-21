module Main where

import Control.Monad.State

import Engine
import Draw
import Knobs
import Scene
-- import Event
import qualified Graphics.UI.GLFW as G

main :: IO ()
main = do 
    knobsRef <- watchAsync [] "knobs.txt"
    runEngine def (GameState 0.5 knobsRef sceneMgr) update render input

sceneMgr :: SceneManager GameState
sceneMgr = SceneManager [("a", sceneA), ("b", sceneB)] "b"

sceneA :: Scene GameState
sceneA = Scene {
    updateFn = passTime,
    renderFn = return (),
    inputFn = ignoreInput
}

sceneB :: Scene GameState
sceneB = Scene {
    updateFn = reverseTime,
    renderFn = return (),
    inputFn = ignoreInput
}

type KnobsRef = IORef [(String, Double)]

data GameState = GameState {
    t :: Double,
    _knobsRef :: KnobsRef,
    _sceneMgr :: SceneManager GameState
}

-- Key -> Int -> KeyState -> ModifierKeys -> StateT s IO ()
input :: InputFn GameState
input G.Key'E _ _ _ = do s <- get
                         put $ s { t = 0.5 }
input key i keyState modifierKeys = do
    s <- get
    inputToSceneManager (_sceneMgr s) key i keyState modifierKeys

ignoreInput :: InputFn GameState
ignoreInput _ _ _ _ = return ()

update :: UpdateFn GameState
update dt = do s <- get
               updateSceneManager (_sceneMgr s) dt
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
