module Scene(SceneManager(..),
             Scene(..),
             setScene,
             updateSceneManager,
             renderSceneManager) where

import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader

type UpdateFn s = Double -> StateT s IO ()
type RenderFn s = ReaderT s IO ()

data Scene s = Scene {
    updateFn :: UpdateFn s,
    renderFn :: RenderFn s
}

data SceneManager s = SceneManager {
    scenes :: [(String, Scene s)],
    activeScene :: String
}

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
