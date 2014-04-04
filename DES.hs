module DES (desEncrypt, desDecrypt) where

import Data.Char (ord, chr)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Tuple (swap)

generateKeys :: Int -> [Int]
generateKeys key = f (join 9 (key, key)) 4
    where f k len = map (getKey k) [0..(len-1)]
          getKey k i = (shiftR k (10 - i)) .&. 0xFF

expand :: Int -> Int
expand n = foldl (.|.) 0 $ map f [(0x30, -2), (0x4, -3), (0xC, -1), (0x8, 1), (0x3, 0)]
    where f (mask, shift) = (if shift >= 0 then shiftR else shiftL) (n .&. mask) shift

-- split int n into len ints of bitlength k
splitList :: Int -> Int -> Int -> [Int]
splitList bitlength len n = snd $ iterate f (n, []) !! len
    where f (x, xs) = (x `shiftR` bitlength, (x .&. mask) : xs)
          mask      = (1 `shiftL` bitlength) - 1

split :: Int -> Int -> (Int, Int)
split k n = (left, right)
    where left:right:_ = splitList k 2 n

-- concatenate a list of ints of bitlength k
joinList :: Int -> [Int] -> Int
joinList k = foldl (\a b -> (a `shiftL` k) .|. b) 0

join :: Int -> (Int, Int) -> Int
join k (a, b) = joinList k [a, b]

-- 8 bits -> 6 bits
sBoxLookup :: Int -> Int
sBoxLookup n = join 3 (s1 !! l, s2 !! r)
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
chunk = concat . map joinTriple . group 3 . map ord

-- given 12-bit blocks, turn back into bytes
unchunk = concat . map f . group 2
    where f (a:b:_) = splitList 8 3 $ join 12 (a, b)

desProcessInput f = unchunk . map f . chunk

desEncrypt k = desProcessInput $ desEncryptBlock $ generateKeys k

desDecryptBlock keys n = join 6 $ swap $ foldl desRound (swap $ split 6 n) keys

desDecrypt k = desProcessInput $ desDecryptBlock $ reverse . generateKeys $ k
