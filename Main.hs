{-
	Main file for 'comet', handles commands and provides
	documentation regarding usage.
	
	Author(s):     Lewis Deane
	Last Modified: 18/9/2015
-}

-- Imports
import Control.Applicative
import Data.List (isPrefixOf)
import System.Environment
import System.Directory
import System.IO

import qualified Config as C
import qualified ComTools as T

-- Type synonyms for increased readability.
type FileName = String
type Comment  = String


-- Where everything begins.
main = getArgs >>= parse


-- Allowable settings.
settings :: [String]

settings = ["author", "comment-width"]


-- Parses the input from the command line and handles what should be done.
parse :: [String] -> IO ()

parse [] = doc
parse (x:xs) | x == "s" || x == "set"     = T.setComment     (c !! 0) (c !! 1)
             | x == "a" || x == "append"  = T.appendComment  (c !! 0) (c !! 1)
             | x == "g" || x == "get"     = T.currentComment (c' !! 0)
             | x == "u" || x == "update"  = T.appendComment  (c' !! 0) ""
             | x == "d" || x == "delete"  = T.deleteComment  (c' !! 0)
             | x == "v" || x == "version" = version
             | "set-" `isPrefixOf` x      = configS (drop 4 x) (c' !! 0)
             | "get-" `isPrefixOf` x      = configG (drop 4 x)
             | otherwise                  = putStrLn usage
             where c  = check xs 2
             	   c' = check xs 1

check :: [String] -> Int -> [String]

check params num = if length params == num then params else error $ "Expected " ++ show num ++ " parameters but found " ++ ((show . length) params) ++ ". " ++ usage


usage :: String

usage = "Run 'comet' for a list of legal commands."


-- Launches the appropriate config action.
configS :: String -> String -> IO ()

configS k v = do
  if k `elem` settings then C.writeValue (k, v) else error $ "No such setting '" ++ k ++ "' " ++ usage


-- Gets the current setting from config.
configG :: String -> IO ()

configG k = if k `elem` settings then prettyPrint <$> C.readValue k >>= putStrLn else error $ "No such setting '" ++ k ++ "' " ++ usage


-- Adds a new line before and after a string.
prettyPrint :: String -> String

prettyPrint s = "\n" ++ s ++ "\n"


-- What should be printed out when no args are passed to our inital command.
doc :: IO ()

doc = (putStrLn . unlines) $ ["", "Usage", ""] ++ commands ++ ["\n"] ++ languages ++ [""]


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
		             ("comet get-author           ", "Get author."),
		             ("comet get-comment-width    ", "Get comment width.")]


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
