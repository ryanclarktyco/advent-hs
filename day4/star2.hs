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
    in fst (head (filter hasSixLeadingZeros saltsAndHashes))

combineSecretKeyAndSalt :: String -> Int -> String
combineSecretKeyAndSalt secret_key salt = secret_key ++ (show salt)

calculateHash :: String -> String
calculateHash key = md5s (Str key)

hasSixLeadingZeros :: (Int, String) -> Bool
hasSixLeadingZeros (_, hash)
    | take 6 hash == "000000" = True
hasSixLeadingZeros _ = False
