import DES
import Data.Char (chr)
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
    (action:keyStr:_) <- getArgs
    let key = read keyStr
    input <- B.getLine
    if action == "encrypt"
        then do
            let encrypted = desEncrypt key . B.unpack $ input
            B.putStr . B.pack . map chr $ encrypted
        else
            if action == "decrypt"
                then do
                    let decrypted = desDecrypt key . B.unpack $ input
                    B.putStr . B.pack . map chr $ decrypted
            else
                hPutStrLn stderr "not implemented"
