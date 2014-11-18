module Types where

import Control.Monad.State
import Control.Monad.Reader

type UpdateFn s = Double -> StateT s IO ()
type RenderFn s = ReaderT s IO ()
