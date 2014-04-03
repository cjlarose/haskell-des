import DES
import Data.Char (chr)

main :: IO ()
main = do
    plaintext <- getLine
    let encrypted = desEncrypt 90 plaintext
    putStr . map chr $ encrypted
