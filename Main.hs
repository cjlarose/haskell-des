import DES
import Data.Char (chr)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    plaintext <- B.getLine
    let encrypted = desEncrypt 511 . B.unpack $ plaintext
    B.putStr . B.pack . map chr $ encrypted
