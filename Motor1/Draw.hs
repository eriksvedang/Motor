module Draw(
    line,rgbColor,rgbaColor,withColor,
    white,black,red,green,blue) where

import Graphics.Rendering.OpenGL

type Point = (Double, Double)

rgbColor :: Double -> Double -> Double -> Color3 GLdouble
rgbColor r g b = Color3 (realToFrac r) (realToFrac g) (realToFrac b) :: Color3 GLdouble

rgbaColor :: Double -> Double -> Double -> Double -> Color4 GLclampf
rgbaColor r g b a = Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a) :: Color4 GLclampf

black, white, red, blue, green :: Color3 GLdouble
black = rgbColor 0 0 0
white = rgbColor 1 1 1
red = rgbColor 1 0 0
green = rgbColor 0 0 1
blue = rgbColor 0 1 0

withColor :: Color3 GLdouble -> IO () -> IO ()
withColor rgb renderingFunction = do
    color rgb
    renderingFunction

line :: Point -> Point -> IO ()
line (x1,y1) (x2,y2) =
    renderPrimitive Lines $ do
        vertex (Vertex3 (realToFrac x1) (realToFrac y1) 0 :: Vertex3 GLdouble)
        vertex (Vertex3 (realToFrac x2) (realToFrac y2) 0 :: Vertex3 GLdouble)
