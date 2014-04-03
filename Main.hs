import DES
import Data.Char (chr)
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
    (action:keyStr:_) <- getArgs
    let key = read keyStr
    if action == "encrypt"
        then do
            plaintext <- B.getLine
            let encrypted = desEncrypt key . B.unpack $ plaintext
            B.putStr . B.pack . map chr $ encrypted
        else
            if action == "decrypt"
                then putStr "decrypt not implemented"
            else
                hPutStrLn stderr "not implemented"
