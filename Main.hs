{-
	Main file for 'comet', this is where everything is accessed from.
	
	Author(s):     Alicia
	Last Modified: 14/9/2015
-}

-- Imports
import System.IO
import System.Environment
import System.Directory
import Data.List (isPrefixOf)

import Config as C
import ComTools as T

-- Type synonyms for increased readability.
type FileName = String
type Comment  = String


-- Where everything begins.
main = do
  args <- getArgs
  parse args


-- Allowable settings.
settings :: [String]

settings = ["author", "comment-width"]


-- Parses the input from the command line and handles what should be done.
parse :: [String] -> IO ()

parse [] = doc
parse (x:xs) | x == "s" || x == "set"     = T.setComment (head xs) (last xs)
             | x == "a" || x == "append"  = T.appendComment (head xs) (last xs)
             | x == "g" || x == "get"     = T.currentComment $ head xs
             | x == "d" || x == "delete"  = T.deleteComment $ head xs
             | x == "v" || x == "version" = version
             | "set-" `isPrefixOf` x      = configS (drop 4 x) (head xs)
             | "get-" `isPrefixOf` x      = configG $ drop 4 x
             | otherwise                  = putStrLn "Incorrect usage - run 'comet' for usage details."


-- Launches the appropriate config action.
configS :: String -> String -> IO ()

configS k v = do
  if k `elem` settings then C.setValue (k, v) else error $ "No such setting '" ++ k ++ "'. Run 'comet' for a list of legal commands."


-- Gets the current setting from config.
configG :: String -> IO ()

configG s = do
	v <- C.getValue s
	(putStrLn . prettyPrint) v


-- Adds a new line before and after a string.
prettyPrint :: String -> String

prettyPrint s = "\n" ++ s ++ "\n"


-- What should be printed out when no args are passed to our inital command.
doc :: IO ()

doc = (putStrLn . unlines) (["", "Usage", ""] ++ commands ++ ["\n"] ++ languages ++ [""])


-- List of commands to be outputted when 'comet' is run.
commands :: [String]

commands = zipWith3 (concat3) x (repeat "\t") y
          where x = map fst c
                y = map snd c
                c = [("COMMAND                    ", "DESCRIPTION"),
		             ("comet s|set file comment   ", "Write comment to file."),
		             ("comet a|append file comment", "Append comment to file."),
		             ("comet d|delete             ", "Delete comment from file."),
		             ("comet g|get file           ", "Get comment from file."),
		             ("comet v|version            ", "Get current version."),
		             ("comet set-author name      ", "Set author to name."),
		             ("comet set-comment-width num", "Set comment width to num."),
		             ("comet get-author           ", "Get author."),
		             ("comet get-comment-width    ", "Get comment width.")]


-- Nicely formats allowed files and extensions.
languages :: [String]

languages = zipWith3 (concat3) x (repeat "\t") y
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

version = putStrLn "v0.1"
