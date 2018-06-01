module Core.ByteString where 

-- import Data.ByteString hiding pack as B
import Data.List as L
import Data.ByteString.Char8 as B
import Data.Hex as H 
import Data.Char as C 
import Crypto.Hash as CH

sha1 :: ByteString -> Digest SHA1
sha1 = hash

-- hex2sha1 x = hash $ pack $ unhex x

sha3_512 :: ByteString -> Digest SHA3_512
sha3_512 = hash


-- hex == Data.Hex.hex

str2int :: String -> [Int]
str2int = hex2int . hex

hex2int :: String -> [Int]
hex2int = L.map digitToInt


extract x = if x==fail "" then error "extract failed." else do r <- x ; r
limit2hex x = unhex . hex $ x 

-- hex 
-- A |-> 41
-- Z |-> 5A
--
-- a |-> 61 
-- z |-> 7A
--


