import DES (desEncryptBlock, desDecryptBlock, generateKeys)

key = "101101001"
plaintext = "101010101010"

bitstringToInt :: String -> Int
bitstringToInt xs = foldl f 0 xs
    where
        f sum '0' = sum * 2
        f sum '1' = sum * 2 + 1

intToBitstring n = reverse $ f n
f x | x == 0         = ""
    | x `mod` 2 == 0 = "0" ++ f (x `div` 2)
    | otherwise      = "1" ++ f (x `div` 2)

main :: IO ()
main = do
    let k = bitstringToInt key
    let encrypted = desEncryptBlock (generateKeys k) (bitstringToInt plaintext)
    putStrLn $ "Encrypted : " ++ intToBitstring encrypted

    let decrypted = desDecryptBlock (reverse $ generateKeys k) encrypted
    putStrLn $ "Decrypted : " ++ intToBitstring decrypted
