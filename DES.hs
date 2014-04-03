module DES (desEncrypt) where

import Data.Char (ord, chr)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Tuple (swap)

generateKeys :: Int -> [Int]
generateKeys key = f (join (key, key) 9) 4
    where f k len = map (getKey k) [0..(len-1)]
          getKey k i = (shiftR k (10 - i)) .&. 255

expand :: Int -> Int
expand n = (shiftL (n .&. 0x30) 2) .|. 
           (shiftL (n .&. 0x4 ) 3) .|. 
           (shiftL (n .&. 0xC) 1) .|. 
           (shiftR (n .&. 0x8) 1) .|.
           (n .&. 0x3)

-- split int n into len ints of bitlength k
splitList :: Int -> Int -> Int -> [Int]
splitList n len bitlength = snd $ foldl f (n, []) [0..(len-1)]
    where f (x, xs) _ = (x `shiftR` bitlength, (x .&. mask) : xs)
          mask        = (1 `shiftL` bitlength) - 1

split :: Int -> Int -> (Int, Int)
split n k = (left, right)
    where left:right:_ = splitList n 2 k

-- concatenate a list of ints of bitlength k
joinList :: [Int] -> Int -> Int
joinList xs k = foldl (\a b -> (a `shiftL` k) .|. b) 0 xs

join :: (Int, Int) -> Int -> Int
join (a, b) k = joinList [a, b] k

-- 8 bits -> 6 bits
sBoxLookup :: Int -> Int
sBoxLookup n = join (s1 !! l, s2 !! r) 3
    where (l, r) = split n 4
          s1 = [5, 2, 1, 6, 3, 4, 7, 0, 1, 4, 6, 2, 0, 7, 5, 3]
          s2 = [4, 0, 6, 5, 7, 1, 3, 2, 5, 3, 0, 7, 6, 2, 1, 4]

desRound (l, r) k_i = (r, newR)
    where newR = (xor l) . sBoxLookup . (xor k_i) . expand $ r

-- encrypt 12 bit plaintex n with keys
desEncryptBlock keys n = join (foldl desRound (split n 6) keys) 6

group _ [] = []
group n l
    | n > 0 = (take n l) : (group n (drop n l))
    | otherwise = error "Negative n"

joinTriple triple = [a,b]
    where (a, b) = split (joinList triple 8) 12

-- chunk input characters into 12-bit chunks
chunk xs = concat . map joinTriple . group 3 . map ord $ xs

-- given 12-bit blocks, turn back into bytes
unchunk = concat . map f . group 2
    where f (a:b:_) = splitList (join (a, b) 12) 3 8

desProcessInput f = unchunk . map f . chunk

desEncrypt k = desProcessInput $ desEncryptBlock $ generateKeys k


--desRoundFlip n k_i = join (r, newR) 6
--    where (r, l) = split n 6
--          newR = (xor l) . sBoxLookup . (xor k_i) . expand $ r
--
--desDecryptBlock keys n = foldl desRoundFlip n keys
--    --where reverseChunk = join (swap $ split n 6) 6
--
--desDecrypt k = desProcessInput $ desDecryptBlock ((reverse . generateKeys) k)