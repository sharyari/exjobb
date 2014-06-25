module DataTypes where
--import Data.Set as S
import Data.Word as W
import Data.ByteString as B
import Data.Trie as T
import Data.Set as S

-- Basic datatype used to work with configurations
-- The bytestring is the set of states, the string list is the channel evaluation
data C = Conf ByteString [String] | Null deriving (Show, Eq, Ord)

type CTrie = Trie (Set ([String], Bool))
type TNode = (Set ([String], Bool))
type NodeElem = ([String], Bool)

instance Show CTrie where
  show c = "hej"

-- Basic datatype to apply rules
-- The bytestring is the new bytestring, the tuple (chNum, op, symbol) is a read or write operation such that
-- op = ? => if the chNum:th channel contains the symbol, remove the symbol
-- op = ! => append the symbol to the end of the chNum:th channel
-- op = ¡ => append the symbol to the beginning of the chNum:th channel (used for stacks/lifo)
-- op = - => leave channels unchanged
data R = Rule ByteString (Int, String, String) deriving (Show, Eq, Ord)

