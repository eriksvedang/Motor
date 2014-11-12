module Scene(SceneManager(..),
             Scene(..),
             setScene,
             updateSceneManager,
             renderSceneManager) where

import Data.Maybe
import Event

-- g = game state type
-- k = knobs type

data Scene g k = Scene {
    renderFn :: g -> k -> IO (),
    updateFn :: g -> k -> Double -> (g, [Event])
}

data SceneManager g k = SceneManager {
    scenes :: [(String, Scene g k)],
    activeScene :: String
}

setScene :: SceneManager g k -> String -> SceneManager g k
setScene sceneManager key = sceneManager { activeScene = key }

useSceneManager :: (Scene g k -> g -> t) -> SceneManager g k -> g -> t
useSceneManager selector sceneManager gameState = 
    let key = activeScene sceneManager
        currentScene = fromMaybe (error $ "Scene \"" ++ key ++ "\" not found")
                                 (lookup key $ scenes sceneManager)
        f = selector currentScene
    in f gameState

renderSceneManager :: SceneManager g k -> g -> k -> IO ()
renderSceneManager = useSceneManager renderFn

updateSceneManager :: SceneManager g k -> g -> k -> Double -> (g, [Event])
updateSceneManager = useSceneManager updateFn

