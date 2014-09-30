module DataTypes where
import Data.Word as W
import Data.ByteString as B
import Data.HashSet as H
import Data.HashMap.Strict as M

-- State: the state of a configuration
type State = B.ByteString
-- Eval: The evaluation of a set of channels
type Eval = [CWord]
-- CWord: the word of a single channel
type CWord = [Word8]
-- C: A configuration is a state-evaluation tuple
type C = (State, Eval)

-- Symbols: Each channel has its list of symbols that it may contain
type Symbols = [[Word8]]

-- CMap: A hashmap containing configurations, stored as equal-state configurations
--       indexed by state. 
type CMap = HashMap State MapNode
-- MapNode: A set of evaluations
type MapNode = HashSet Eval

-- CMap2: A Hashmap of configurations, storing the parent node of configurations
--        This is used when computing the forward-reachability, in order to allow
--        a minimal trace to be computed. 
type CMap2 = HashMap C C



-- Basic datatype to apply rules
-- The bytestring is the new bytestring, the tuple (chNum, op, symbol) is a read or write operation such that
-- op = ? => if the chNum:th channel contains the symbol at its end, remove the symbol
-- op = ! => append the symbol to the beginning of the chNum:th channel
-- op = ¡ => append the symbol to the end of the chNum:th channel (used for stacks/lifo)
-- op = - => leave channels unchanged
type Rule = (State, (Int, String, [Word8]))
type RuleMap = HashMap State [Rule]
