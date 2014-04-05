import DES (BlockCipher(..), DES(..))
import Data.Char (chr)
import qualified Data.ByteString as B
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
    (action:keyStr:_) <- getArgs
    let key = read keyStr
    let cipher = DES {rawKey=key}
    input <- B.getContents
    if action == "encrypt"
        then do
            B.putStr $ ecb cipher input
        else
            if action == "decrypt"
                then do
                    B.putStr $ unEcb cipher input
            else
                hPutStrLn stderr "not implemented"
