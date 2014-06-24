module DataTypes where
--import Data.Set as S
import Data.Word as W
import Data.ByteString as B


-- Basic datatype used to work with configurations
-- The bytestring is the set of states, the string list is the channel evaluation
data C = Conf ByteString [String] | Null deriving (Show, Eq, Ord)

-- Basic datatype to apply rules
-- The bytestring is the new bytestring, the tuple (chNum, op, symbol) is a read or write operation such that
-- op = ? => if the chNum:th channel contains the symbol, remove the symbol
-- op = ! => append the symbol to the end of the chNum:th channel
-- op = ¡ => append the symbol to the beginning of the chNum:th channel (used for stacks/lifo)
-- op = - => leave channels unchanged
data R = Rule ByteString (Int, String, String)

-- Help function to create empty configuration from int-list. Only needed for initial configuration and debugging
toConf :: [Word8] -> C
toConf l = Conf (pack l) ["", ""]


initial = toConf ([1,1])