module Scene(SceneManager(..),
             Scene(..),
             setScene,
             updateSceneManager,
             renderSceneManager) where

import Data.Maybe

data Scene g = Scene {
    renderFn :: g -> IO (),
    updateFn :: g -> g
}

data Eq k => SceneManager k g = SceneManager {
    scenes :: [(k, Scene g)],
    activeScene :: k
}

setScene :: (Eq k) => SceneManager k g -> k -> SceneManager k g
setScene sceneManager key = sceneManager { activeScene = key }

useSceneManager :: (Eq k) => (Scene g -> g -> t) -> SceneManager k g -> g -> t
useSceneManager selector sceneManager gameState = 
    let key = activeScene sceneManager
        currentScene = fromMaybe (error "Scene not found") (lookup key $ scenes sceneManager)
        f = selector currentScene
    in f gameState


updateSceneManager :: (Eq k) => SceneManager k g -> g -> g
updateSceneManager = useSceneManager updateFn

renderSceneManager :: (Eq k) => SceneManager k g -> g -> IO ()
renderSceneManager = useSceneManager renderFn
