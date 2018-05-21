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
insertList a = insertList' a empty where
    insertList' :: [Word] -> Trie -> Trie
    insertList' (x:xs) trie
        | xs == [] = insert x trie
        | otherwise = insertList' xs (insert x trie)

search :: Word -> Trie -> Bool
search [] (Trie end child) = end
search (x:xs) (Trie end child) =
    case Map.lookup x child of
        Nothing -> False
        Just trie -> search xs trie

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix [] trie = Just (getWords trie)
prefix a t = prefix' a a t where
    prefix' :: Word -> Word -> Trie -> Maybe [Word]
    prefix' a [] trie = getTrieWords a (getWords trie)
    prefix' a (x:xs) (Trie end child) =
      case Map.lookup x child of
          Nothing -> Nothing
          Just trie -> prefix' a xs trie

getTrieWords :: Word -> [Word] -> Maybe [Word]
getTrieWords _ [] = Nothing
getTrieWords a xs = Just [a ++ x | x <- xs]
