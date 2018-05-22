module Main where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: Map Char Trie} deriving (Eq, Show)
type Word = String

empty :: Trie
empty = Trie {end = False, children = Map.empty}

insert :: Word -> Trie -> Trie
insert [] trie = trie {end = True}
insert (x:xs) (Trie end child) =
    case Map.lookup x child of
        Nothing -> Trie {end = end, children = Map.insert x (insert xs empty) child}
        Just trie -> Trie {end = end, children = Map.insert x (insert xs trie) child}


insertList :: [Word] -> Trie
insertList words = foldr (insert) empty words

search :: Word -> Trie -> Bool
search [] (Trie end child) = end
search (x:xs) (Trie end child) =
    case Map.lookup x child of
        Nothing -> False
        Just trie -> search xs trie

getWords :: Trie -> [Word]
getWords (Trie end child) = if end then [] : words else words
  where
    words = [words | (x,xs) <- Map.toList child, words <- map (x:) $ getWords xs]

prefix :: Word -> Trie -> Maybe [Word]
prefix [] trie = Just (getWords trie)
prefix a t = prefix' a a t where
    prefix' :: Word -> Word -> Trie -> Maybe [Word]
    prefix' a [] trie = getTrieWords a $ getWords trie
    prefix' a (x:xs) (Trie end child) =
      case Map.lookup x child of
          Nothing -> Nothing
          Just trie -> prefix' a xs trie

getTrieWords :: Word -> [Word] -> Maybe [Word]
getTrieWords _ [] = Nothing
getTrieWords a xs = Just [a ++ x | x <- xs]

appLifeCyle :: Trie -> IO()
appLifeCyle trie = do
    prepareUI
    (action:_) <- getLine
    handleAction action trie

prepareUI :: IO()
prepareUI = do
    putStrLn ""
    putStrLn "################################"
    putStrLn "### TRIE TREE DATA STRUCTURE ###"
    putStrLn "################################"
    putStrLn "a) Add Word"
    putStrLn "s) Search Word"
    putStrLn "f) Find words with prefix"
    putStrLn "p) Print all words"
    putStrLn "e) Exit"
    putStrLn "Enter the action:"

handleAction :: Char -> Trie -> IO()
handleAction action trie
    | action == 'e' = putStrLn "Have a nice day :) See you!"
    | action == 'p' = do
        putStrLn "List of words in dictionary:"
        mapM_ putStrLn $ getWords trie
        appLifeCyle trie
    | otherwise = do
        putStrLn "Enter word/prefix:"
        word <- getLine
        handleAction' action word trie where
            handleAction' :: Char -> Word -> Trie -> IO()
            handleAction' action word trie
                | action == 'a' = do
                    putStrLn "New word is added!"
                    appLifeCyle $ insert word trie
                | action == 's' = do
                    if search word trie then putStrLn "Exists in dictionary!"
                        else putStrLn "NOT exist!"
                    appLifeCyle trie
                | action == 'f' = do
                    case (prefix word trie) of
                        Just words -> do
                            putStrLn "Found words:"
                            mapM_ putStrLn words
                        Nothing -> putStrLn "No words found with that prefix!"
                    appLifeCyle trie

main = do
    -- get command line argument
    (readText: args) <- getArgs

    -- read file and convert to word list
    content <- readFile readText
    let words = lines content

    -- create trie
    let trie = insertList words

    -- Fire App Lifecyle
    appLifeCyle trie
