module Draw (
    line,
    withColor
) where

import Graphics.Rendering.OpenGL

type Point = (Double, Double)

withColor :: Color3 GLdouble -> IO () -> IO ()
withColor rgb renderingFunction = do
    color rgb
    renderingFunction

line :: Point -> Point -> IO ()
line (x1,y1) (x2,y2) =
    renderPrimitive Lines $ do
        vertex (Vertex3 (realToFrac x1) (realToFrac y1) 0 :: Vertex3 GLdouble)
        vertex (Vertex3 (realToFrac x2) (realToFrac y2) 0 :: Vertex3 GLdouble)
