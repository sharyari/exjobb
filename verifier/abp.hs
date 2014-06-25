import DataTypes
import Verifier
import TrieModule
import StringManipulation
import Gamma
import Step
import Alpha


import Data.List
import Data.Trie as T
import Data.Word
import Data.ByteString

myTrie = tAdd T.empty initial False

test1 = alpha(step(gamma(myTrie)))
test2 = alpha(step(gamma test1))
main = skriv (getSize (verify myTrie 50))
--main = skriv (getSize myTrie)
--main = skriv (T.lookup (pack[1,1,1]) rules)
