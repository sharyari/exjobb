module DataTypes where
--import Data.Set as S
import Data.Word as W
import Data.ByteString as B
import Data.Trie as T
import Data.HashSet as H
import Data.HashMap.Strict as M

-- Basic datatype used to work with configurations
-- The bytestring is the set of states, the string list is the channel evaluation
type C = (State, Eval)


type CWord = [Word8]
type State = B.ByteString
type Eval = [CWord]
type Symbols = [[Word8]]

-- Short name for a configuration tree
type CMap = HashMap State MapNode
-- Short name for the nodes of a CTrie
type MapNode = HashSet Eval


-- Basic datatype to apply rules
-- The bytestring is the new bytestring, the tuple (chNum, op, symbol) is a read or write operation such that
-- op = ? => if the chNum:th channel contains the symbol at its end, remove the symbol
-- op = ! => append the symbol to the beginning of the chNum:th channel
-- op = ¡ => append the symbol to the end of the chNum:th channel (used for stacks/lifo)
-- op = - => leave channels unchanged
type Rule = (State, (Int, String, [Word8]))
type RuleMap = HashMap State [Rule]
