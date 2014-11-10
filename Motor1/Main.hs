module Main where

import Engine;
import Graphics.Rendering.OpenGL

main :: IO ()
main = runEngine (WindowSettings "MOTOR" (500, 500)) initState render update

type State = Double

initState :: State
initState = 10.0

render :: State -> IO ()
render state = do
    rotate (realToFrac state * 50) (Vector3 0 0 1 :: Vector3 GLdouble)
    renderPrimitive Triangles $ do
        color  (Color3 1 1 0 :: Color3 GLdouble)
        vertex (Vertex3 (negate 0.6) (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 1 0 :: Color3 GLdouble)
        vertex (Vertex3 0.6 (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 1 1 :: Color3 GLdouble)
        vertex (Vertex3 0 0.6 0 :: Vertex3 GLdouble)

update :: Double -> State -> State
update dt state = state + dt * 1.0
