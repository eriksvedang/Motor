module Colors(
    rgbColor,rgbaColor,
    white,black,red,green,blue) where

import Graphics.Rendering.OpenGL

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
