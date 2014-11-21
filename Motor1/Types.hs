module Types where

import Control.Monad.State
import Control.Monad.Reader
import Graphics.UI.GLFW

type UpdateFn s = Double -> StateT s IO ()
type RenderFn s = ReaderT s IO ()
type InputFn s = Key -> Int -> KeyState -> ModifierKeys -> StateT s IO ()
