{-
    Handles reading and writing to the config file.
    
    Author(s):     Lewis Deane
    License:       MIT
    Last Modified: 18/10/2015
-}

module Config (configName, readValue, writeValue) where

    -- Imports
    import Control.Applicative
    import Data.List
    import Data.List.Split
    import Paths_comet
    import System.Directory
    import System.IO

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
        contents <- lines <$> readFile path

        let f = filter (key `isPrefixOf`) contents
            v = (last . splitOn ":" . head) f

        return v


    -- Sets the value of a key to a value.
    writeValue :: (Key, Value) -> IO ()
    
    writeValue (key, value) = do
        path     <- getDataFileName configName
        contents <- lines <$> readFile path

        let f x = if key `isPrefixOf` x then key ++ ":" ++ value else x
            c = map f contents

        length contents `seq` (writeFile path $ unlines c)
