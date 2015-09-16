{-
	Handles reading and writing to the config file.
	
	Author(s):     Lewis Deane
	Last Modified: 14/9/2015
-}

module Config (getValue, configName, setValue) where

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
    type Line     = String
    type FileName = String

    -- Name of our config file.
    configName :: FileName

    configName = "config.txt"


    -- Gets the value associated with a given key.
    getValue :: Key -> IO Value

    getValue k = do
        f <- getDataFileName "config.txt"
        c <- lines <$> readFile f

        let func = b k
            b k' y [] = y
            b k' y x  = if (head . s) x == k' then (last . s) x else y
            s = splitOn ":"

        return $ foldl func "" c


    -- Sets the value of a key to a value.
    setValue :: (Key, Value) -> IO ()
    
    setValue (k, v) = do
        f <- getDataFileName "config.txt"
        c <- lines <$> readFile f

        let func = b (k, v)
            b (k', v') y [] = y
            b (k', v') y x  = if (head . splitOn ":") x == k' then y ++ [k' ++ ":" ++ v'] else y ++ [x]

        length c `seq` ((writeFile f . unlines . foldl func []) c) -- We use this hack here because we want to write to the file after reading from it.
