import DES
import Data.Char (chr)
import qualified Data.ByteString as B
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
    (action:keyStr:_) <- getArgs
    let key = read keyStr
    input <- B.getContents
    if action == "encrypt"
        then do
            let encrypted = desEncrypt key . map fromIntegral . B.unpack $ input
            B.putStr . B.pack . map fromIntegral $ encrypted
        else
            if action == "decrypt"
                then do
                    let decrypted = desDecrypt key . map fromIntegral . B.unpack $ input
                    B.putStr . B.pack . map fromIntegral $ decrypted
            else
                hPutStrLn stderr "not implemented"
