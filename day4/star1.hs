import System.Environment
import Data.Hash.MD5

main :: IO ()
main = do
    (secret_key:_) <- getArgs
    print (calculateLowestSalt secret_key)

calculateLowestSalt :: String -> Int
calculateLowestSalt secret_key =
    let salts = [1..]
        totalKeys = map (combineSecretKeyAndSalt secret_key) salts
        saltsAndHashes = zip salts (map calculateHash totalKeys)
    in fst (head (filter hasFiveLeadingZeros saltsAndHashes))

combineSecretKeyAndSalt :: String -> Int -> String
combineSecretKeyAndSalt secret_key salt = secret_key ++ (show salt)

calculateHash :: String -> String
calculateHash key = md5s (Str key)

hasFiveLeadingZeros :: (Int, String) -> Bool
hasFiveLeadingZeros (_, hash)
    | take 5 hash == "00000" = True
hasFiveLeadingZeros _ = False
