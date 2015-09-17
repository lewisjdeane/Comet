{-
	Handles reading and writing to the config file.
	
	Author(s):     Lewis Deane
	Last Modified: 14/9/2015
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

        let f = f' key
            s = splitOn ":"
            f' k y [] = y
            f' k y x  = if (head . s) x == k then (last . s) x else y

        return $ foldl f "" contents


    -- Sets the value of a key to a value.
    writeValue :: (Key, Value) -> IO ()
    
    writeValue (key, value) = do
        path     <- getDataFileName configName
        contents <- lines <$> readFile path

        let f = f' (key, value)
            s = splitOn ":"
            f' (k, v) x = if (head . s) x == k then k ++ ":" ++ v else x
            c = map f contents

        length contents `seq` (writeFile path $ unlines c)