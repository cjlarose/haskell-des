module DES (BlockCipher(..), DES(..)) where

import Data.Char (ord, chr)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor, Bits)
import Data.Tuple (swap)
import Crypto.Classes (BlockCipher(..))
import Data.Tagged (Tagged(..))
import Data.Serialize (Serialize(..), putWord16be, getByteString)
import Data.Word (Word16)
import qualified Data.ByteString as B

generateKeys key = f (join 9 (key, key)) 4
    where f k len = map (getKey k) [0..(len-1)]
          getKey k i = (shiftR k (10 - i)) .&. 0xFF

expand n = (shiftL (n .&. 0x30) 2) .|. 
           (shiftL (n .&. 0x4 ) 3) .|. 
           (shiftL (n .&. 0xC) 1) .|. 
           (shiftR (n .&. 0x8) 1) .|.
           (n .&. 0x3)

-- split int n into len ints of bitlength k
splitList bitlength len n = map f [0..(len-1)]
    where f i = n `shiftR` ((len - i - 1) * bitlength)  .&. mask
          mask    = (1 `shiftL` bitlength) - 1

split k n = (left, right)
    where left:right:_ = splitList k 2 n

-- concatenate a list of ints of bitlength k
joinList k = foldl (\a b -> (a `shiftL` k) .|. b) 0

join k (a, b) = joinList k [a, b]

-- 8 bits -> 6 bits
sBoxLookup n = join 3 (s1 !! (fromIntegral l), s2 !! (fromIntegral r))
    where (l, r) = split 4 n
          s1 = [5, 2, 1, 6, 3, 4, 7, 0, 1, 4, 6, 2, 0, 7, 5, 3]
          s2 = [4, 0, 6, 5, 7, 1, 3, 2, 5, 3, 0, 7, 6, 2, 1, 4]

desRound (l, r) k_i = (r, newR)
    where newR = xor l . sBoxLookup . xor k_i . expand $ r

-- encrypt 12 bit plaintex n with keys
desEncryptBlock keys n = join 6 $ foldl desRound (split 6 n) keys

group _ [] = []
group n l
    | n > 0 = (take n l) : (group n (drop n l))
    | otherwise = error "Negative n"

joinTriple triple = [a,b]
    where (a, b) = split 12 $ joinList 8 triple

-- chunk input characters into 12-bit chunks
chunk :: (Num t, Bits t) => [t] -> [t]
chunk = concat . map joinTriple . group 3

-- given 12-bit blocks, turn back into bytes
unchunk :: (Num t, Bits t) => [t] -> [t]
unchunk = concat . map f . group 2
    where f (a:b:_) = splitList 8 3 $ join 12 (a, b)

desProcessInput f = unchunk . map f . chunk

--desEncrypt :: Int -> [Int] -> [Int]
desEncrypt k = desProcessInput $ desEncryptBlock $ generateKeys k

desDecryptBlock keys n = join 6 $ swap $ foldl desRound (swap $ split 6 n) keys

--desDecrypt :: Int -> [Int] -> [Int]
desDecrypt k xs = desProcessInput (desDecryptBlock . reverse . generateKeys $ k) xs

data DES = DES { rawKey :: Word16 } deriving Show

instance Serialize DES where
    put k = do
        putWord16be (rawKey k)
    get = do
        b <- getByteString 2
        case buildKey b of
            Nothing -> fail "Invalid key on 'get'"
            Just k -> return k

instance BlockCipher DES where
    blockSize = Tagged 24
    keyLength = Tagged 9
    encryptBlock (DES k) plaintext = B.pack . map fromIntegral . desEncrypt k . map fromIntegral . B.unpack $ plaintext
    decryptBlock (DES k) ciphertext = B.pack . map fromIntegral . desDecrypt k . map fromIntegral . B.unpack $ ciphertext
    buildKey k = Just $ DES $ head . map fromIntegral $ B.unpack k
