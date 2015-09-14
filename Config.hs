{-
	Handles reading and writing to the config file.
	
	Author(s):     Lewis Deane
	Last Modified: 14/9/2015
-}

module Config (genConfig, genConfigWithInitial, getValue, configName, setValue) where

    -- Imports
    import System.IO
    import Control.Applicative
    import System.Directory
    import Data.List
    import Data.List.Split
    import Paths_comet

    -- Some type synoyms to make types easier to read.
    type Key      = String
    type Value    = String
    type Line     = String
    type FileName = String

    -- Name of our config file.
    configName :: FileName

    configName = "config.txt"
    
    -- Generates a new blank config file
    genConfig :: IO ()

    genConfig = writeFile configName ""


    -- Generates a new config file with initial data
    genConfigWithInitial :: (Key, Value) -> IO ()

    genConfigWithInitial (k, v) = writeFile configName $ k ++ ":" ++ v


    -- Gets the value associated with a given key.
    getValue :: Key -> IO Value

    getValue k = do
        f <- getDataFileName "config.txt"
        c <- lines <$> readFile f

        let v = values c
            i = if f == Nothing then error "No such key!" else extract f where f = findIndex (== k) $ keys c

        return $ v !! i


    -- Sets the value of a key to a value.
    setValue :: (Key, Value) -> IO ()
    
    setValue (k, v) = do
        f <- getDataFileName "config.txt"
        c <- lines <$> readFile f

        let n = k ++ ":" ++ v

        let p = if f == Nothing then n : c else take i c ++ [n] ++ drop (succ i) c
                where i = extract f
                      f = findIndex (== k) $ keys c

        length c `seq` (writeFile f $ unlines p) -- We use this hack here because we want to write to the file after reading from it.


    -- Turns a list of lines into a list of keys.
    keys :: [Line] -> [Value]

    keys = map (head . splitOn ":")


    -- Turns a list of lines into a list of values.
    values :: [Line] -> [Value]

    values = map (last . splitOn ":")


    -- Tries to extract value from maybe and if can't then throws error.
    extract :: Maybe Int -> Int

    extract (Just x) = x
    extract _        = error "Not found."
