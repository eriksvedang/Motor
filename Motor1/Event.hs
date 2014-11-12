module Event where

data Event = ChangeScene String
           | Quit
           | SetWindowSize (Int,Int)