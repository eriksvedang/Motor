{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

module Knobs(
    IORef,
    readSetting,
    watchAsync
) where

import Control.Monad (when, forever)
import System.FSNotify
import Filesystem.Path.CurrentOS
import Control.Concurrent (threadDelay, forkIO)
import System.IO (hClose, openFile, IOMode(..), hGetContents)
import Data.IORef
import Data.Text
import Data.Maybe

wasItModified :: ActionPredicate
wasItModified (Modified _ _) = True
wasItModified _ = False

readKnobsSettings :: Read a => String -> IORef a -> IO ()
readKnobsSettings path knobs = do
    handle <- openFile path ReadMode  
    contents <- hGetContents handle
    putStrLn $ " - New knobs - \n" ++ contents
    let newValue = read contents
    writeIORef knobs newValue
    hClose handle

onModified :: Read a => IORef a -> String -> Action
onModified knobs knobsFile (Modified path _) = do
    --putStrLn $ show path ++ " was modified"
    let (Right pathAsText) = toText $ filename path
    when (unpack pathAsText == knobsFile) $ readKnobsSettings (encodeString path) knobs
onModified _ _ _ = return ()

startWatching :: Read a => IORef a -> String -> IO ()
startWatching knobs knobsFile =
    withManager $ \mgr -> do
        _ <- watchDir mgr "." wasItModified (onModified knobs knobsFile)
        forever $ threadDelay 5

watchAsync :: Read k => k -> String -> IO (IORef k)
watchAsync defaultKnobs knobsFile = do
    knobsRef <- newIORef defaultKnobs
    readKnobsSettings knobsFile knobsRef
    _ <- forkIO (startWatching knobsRef knobsFile)
    return knobsRef

readSetting :: Eq a => a -> IORef [(a, b)] -> IO b
readSetting key ref = do value <- readIORef ref
                         return $ fromJust $ lookup key value
