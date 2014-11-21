module Scene(SceneManager(..),
             Scene(..),
             setScene,
             updateSceneManager,
             renderSceneManager,
             inputToSceneManager
             ) where

import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Types
import Graphics.UI.GLFW (Key, KeyState, ModifierKeys)

data Scene s = Scene {
    updateFn :: UpdateFn s,
    renderFn :: RenderFn s,
    inputFn  :: InputFn s
}

instance Show (Scene s) where
    show _ = "Scene"

data SceneManager s = SceneManager {
    scenes :: [(String, Scene s)],
    activeScene :: String
}

instance Show (SceneManager s) where
    show mgr = "SceneManager: " ++ activeScene mgr

setScene :: SceneManager s -> String -> SceneManager s
setScene sceneManager key = sceneManager { activeScene = key }

getActiveScene :: SceneManager s -> Scene s
getActiveScene sceneManager = 
    let key = activeScene sceneManager
    in fromMaybe (error $ "Scene \"" ++ key ++ "\" not found")
                 (lookup key $ scenes sceneManager)

updateSceneManager :: SceneManager s -> Double -> StateT s IO ()
updateSceneManager mgr dt =
    let scene = getActiveScene mgr
        f = updateFn scene
    in f dt

renderSceneManager :: SceneManager s -> ReaderT s IO ()
renderSceneManager mgr =
    let scene = getActiveScene mgr
        f = renderFn scene
    in f

inputToSceneManager :: SceneManager s -> Key -> Int -> KeyState -> ModifierKeys -> StateT s IO ()
inputToSceneManager mgr key i keyState modifierKeys =
    let scene = getActiveScene mgr
        f = inputFn scene
    in f key i keyState modifierKeys
