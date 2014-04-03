import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)

s1 = [5, 2, 1, 6, 3, 4, 7, 0, 1, 4, 6, 2, 0, 7, 5, 3]
s2 = [4, 0, 6, 5, 7, 1, 3, 2, 5, 3, 0, 7, 6, 2, 1, 4]

generateKeys :: Int -> [Int]
generateKeys key = f ((key `shiftL` 9) .|. key) 4
    where f k len = map (getKey k) [0..(len-1)]
          getKey k i = shiftR k (10 - i)

expand :: Int -> Int
expand n = (shiftL (n .&. 0x30) 2) .|. 
           (shiftL (n .&. 0x4 ) 3) .|. 
           (shiftL (n .&. 0xC) 1) .|. 
           (shiftR (n .&. 0x8) 1) .|. 
           (n .&. 0x3)

-- splits int n into two ints of bitlength k
split :: Int -> Int -> (Int, Int)
split n k = (left,  right)
    where left = n `shiftR` k
          right = n .&. (( 1 `shiftL` k ) - 1)

-- concatenate a pair of ints of bitlength k
join :: (Int, Int) -> Int -> Int
join (a, b) k = (a `shiftL` k) .|. b

-- 8 bits -> 6 bits
sBoxLookup :: Int -> Int
sBoxLookup n = join (s1 !! l, s2 !! r) 3
    where (l, r) = split n 4

dsaRound n k_i = join (r, newR) 6
    where (l, r) = split n 6
          newR = (xor l) . sBoxLookup . (xor k_i) . expand $ r
