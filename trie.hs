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
insert (x:xs) (Trie e child) =
    case Map.lookup x child of
        Nothing -> Trie {end = e, children = Map.insert x (insert xs empty) child}
        Just trie -> Trie {end = e, children = Map.insert x (insert xs trie) child}


insertList :: [Word] -> Trie
insertList = undefined

search :: Word -> Trie -> Bool
search = undefined

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined
