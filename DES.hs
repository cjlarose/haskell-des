module DES (BlockCipher(..), DES(..)) where

import Data.Char (ord, chr)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor, Bits)
import Data.Tuple (swap)
import Crypto.Classes (BlockCipher(..))
import Data.Tagged (Tagged(..))
import Data.Serialize (Serialize(..), putWord32be, getByteString)
import Data.Word (Word16, Word32)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Strict.BitGet (getAsWord16, runBitGet, BitGet(..))
import Data.Binary.BitPut (BitPut(..), runBitPut, putNBits)

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
desEncryptBlock :: (Integral b, Bits b) => [b] -> b -> b
desEncryptBlock keys n = join 6 $ foldl desRound (split 6 n) keys

desDecryptBlock keys n = join 6 $ swap $ foldl desRound (swap $ split 6 n) keys

-- list of 12-bit blocks -> list of 12-bit blocks
desEncrypt' :: (Integral b, Bits b) => b -> [b] -> [b]
desEncrypt' k bs = map f bs
    where f = desEncryptBlock (generateKeys k)

desDecrypt' :: (Integral b, Bits b) => b -> [b] -> [b]
desDecrypt' k bs = map f bs
    where f = desDecryptBlock (generateKeys k)

getBlocks :: BitGet (Word16, Word16)
getBlocks = do
    a <- getAsWord16 12
    b <- getAsWord16 12
    return (a, b)

--getBlocks' :: B.ByteString -> Either String (Word16, Word16)
getBlocks' input = runBitGet input getBlocks

putBlock :: Word16 -> BitPut
putBlock = putNBits 12

--putBlocks :: [Word16] -> B.ByteString
--putBlocks bs = BL.toChunks . BL.concat $ map (runBitPut . putBlock) bs
putBlocks :: [Word16] -> B.ByteString
putBlocks bs = B.concat . concat $ map (BL.toChunks . runBitPut . putBlock) bs

data DES = DES { rawKey :: Word32 } deriving Show

instance Serialize DES where
    put k = do
        putWord32be (rawKey k)
    get = do
        b <- getByteString 4
        case buildKey b of
            Nothing -> fail "Invalid key on 'get'"
            Just k -> return k

desProcessInput f input = putBlocks . map fromIntegral . f . map fromIntegral $ [a, b]
    where (a, b) = case (getBlocks' input) of { Right x -> x; Left msg -> error msg }

instance BlockCipher DES where
    blockSize = Tagged 24
    keyLength = Tagged 9
    encryptBlock (DES k) plaintext = desProcessInput (desEncrypt' k) plaintext
    decryptBlock (DES k) ciphertext = desProcessInput (desDecrypt' k) ciphertext
    buildKey k = Just $ DES $ head . map fromIntegral $ B.unpack k
