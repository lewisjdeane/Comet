{-
	Main file for 'comet', handles commands and provides documentation regarding usage.
	
	Author(s):     run 'comet set-author NAME' to change this setting.
	License:       MIT
	Last Modified: 3/10/2015
-}

-- Imports
import Control.Applicative
import Data.List (isPrefixOf)
import System.Environment
import System.Directory
import System.IO

import qualified Config as C
import qualified CommentTools as T

-- Type synonyms for increased readability.
type FileName = String
type Comment  = String


-- Where everything begins.
main = getArgs >>= parse


-- Allowable settings.
settings :: [String]

settings = ["author", "comment-width", "license"]

{-
	comet -> Shows Documentation.

	comet v -> Shows version.

	comet s FILE COMMENT -> Sets COMMENT to FILE with all fields.
	comet s FILE COMMENT -l -a -> Sets COMMENT to FILE without license (l) and author (a).

	comet a FILE COMMENT -> Appends COMMENT to FILE with current fields.
	comet a FILE COMMENT -l -a -> Appends COMMENT to FILE without the license and author fields.

	comet u FILE -> Updates current fields in FILE with the latest values.
	comet u FILE -l -a -> Updates all fields but license and author with latest values.

	comet d FILE -> Delete the comment header from FILE.

	comet set-SETTING VALUE -> Sets the value of SETTING to VALUE.

	comet get-SETTING -> Gets the current value of SETTING. 
-}

-- TODO: Rewrite this, needs improving.
-- Parses the input from the command line and handles what should be done.
parse :: [String] -> IO ()

parse [] = doc

parse [x] | x == "v" || x == "version" = version
          | "get-" `isPrefixOf` x      = configG (drop 4 x)
          | otherwise                  = putStrLn usage

parse [x, y] | x == "g" || x == "get"    = T.currentComment y
             | x == "d" || x == "delete" = T.deleteComment  y
          	 | "set-" `isPrefixOf` x     = configS (drop 4 x) y
             | otherwise                 = putStrLn usage

parse (x:y:z:xs) | x == "s" || x == "set"    = T.setComment    y z xs
                 | x == "a" || x == "append" = T.appendComment y z xs
                 | otherwise                 = putStrLn usage
                 
parse (x:y:xs) | x == "u" || x == "update" = T.appendComment y "" xs
               | otherwise                 = putStrLn usage


parse _ = putStrLn usage


check :: [String] -> Int -> [String]

check params num = if length params == num then params else error $ "Expected " ++ show num ++ " parameters but found " ++ ((show . length) params) ++ ". " ++ usage


-- Provides a reusable string to be used after errors explaining what the user can do to get help.
usage :: String

usage = "Run 'comet' for a list of legal commands."


-- Launches the appropriate config action.
configS :: String -> String -> IO ()

configS k v = if k `elem` settings then C.writeValue (k, v) else error $ "No such setting '" ++ k ++ "' " ++ usage


-- Gets the current setting from config.
configG :: String -> IO ()

configG k = if k `elem` settings then prettyPrint <$> C.readValue k >>= putStrLn else error $ "No such setting '" ++ k ++ "' " ++ usage


-- Adds a new line before and after a string.
prettyPrint :: String -> String

prettyPrint s = "\n" ++ s ++ "\n"


-- What should be printed out when no args are passed to our inital command.
doc :: IO ()

doc = (putStrLn . unlines) $ [prettyPrint "Usage"] ++ commands ++ ["\n"] ++ languages ++ [""]


-- List of commands to be outputted when 'comet' is run.
commands :: [String]

commands = zipWith3 concat3 x (repeat "\t") y
          where x = map fst c
                y = map snd c
                c = [("COMMAND                    ", "DESCRIPTION"),
		             ("comet s|set FILE COMMENT   ", "Write comment to file."),
		             ("comet a|append FILE COMMENT", "Append comment to file."),
		             ("comet d|delete FILE        ", "Delete comment from file."),
		             ("comet g|get FILE           ", "Get comment from file."),
		             ("comet u|update FILE        ", "Updates file with current settings."),
		             ("comet v|version            ", "Get current version."),
		             ("comet set-author NAME      ", "Set author to name."),
		             ("comet set-comment-width NUM", "Set comment width to num."),
		             ("comet set-license NAME      ","Set license to name."),
		             ("comet get-author           ", "Get author."),
		             ("comet get-comment-width    ", "Get comment width."),
		             ("comet get-license          ", "Get license.")]


-- Nicely formats allowed files and extensions.
languages :: [String]

languages = zipWith3 concat3 x (repeat "\t") y
            where x = map fst l
                  y = map snd l
                  l = [("LANGUAGE    ", "FILE EXTENSION"),
		              ("C            ", ".c .h"),
		              ("C++          ", ".cpp"),
		              ("CoffeeScript ", ".coffee"),
		              ("CSS          ", ".css"),
		              ("ERB          ", ".erb"),
		              ("Go           ", ".go"),
		              ("HAML         ", ".haml"),
		              ("Haskell      ", ".hs"),
		              ("HTML         ", ".html .htm .xhtml"),
		              ("Java         ", ".java"),
		              ("JavaScript   ", ".js"),
		              ("MatLab       ", ".matlab"),
		              ("PHP          ", ".php"),
		              ("Python       ", ".py"),
		              ("R            ", ".r"),
		              ("Ruby         ", ".rb"),
		              ("Scala        ", ".scala"),
		              ("SASS         ", ".sass"),
		              ("SCSS         ", ".scss"),
		              ("XML          ", ".xml")]


-- Joins three strings together.
concat3 :: String -> String -> String -> String

concat3 x y z = x ++ y ++ z


-- Returns the current version number.
version :: IO ()

version = putStrLn "v0.3"
