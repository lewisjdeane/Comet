{-
    Handles reading and writing to the config file.
    
    Author(s):     Lewis Deane
    License:       MIT
    Last Modified: 20/7/2016
-}

module Config (configName, readValue, writeValue) where

    -- Imports
    import Control.Applicative
    import Data.List
    import Data.List.Split
    import Paths_comet
    import System.Directory
    import System.IO
    import qualified System.IO.Strict as SIO

    -- Some type synoyms to make types easier to read.
    type Key      = String
    type Value    = String
    type FileName = String


    -- Name of our config file.
    configName :: FileName

    configName = "config.txt"


    -- Gets the value associated with a given key.
    readValue :: Key -> IO Value

    readValue key = do
        path     <- getDataFileName configName
        handle   <- openFile path ReadMode
        contents <- lines <$> SIO.hGetContents handle

        let f = filter (key `isPrefixOf`) contents
            v = (last . splitOn ":" . head) f

        hClose handle

        return v


    -- Sets the value of a key to a value.
    writeValue :: (Key, Value) -> IO ()
    
    writeValue (key, value) = do
        path     <- getDataFileName configName
        handle   <- openFile path ReadMode
        contents <- lines <$> SIO.hGetContents handle

        let f x = if key `isPrefixOf` x then key ++ ":" ++ value else x
            c   = map f contents

        hClose handle
        
        writeFile path $ unlines c
