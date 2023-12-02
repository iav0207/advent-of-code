import System.Environment
import Data.Char (isDigit)

main :: IO ()
main = do
    args <- getArgs
    let debugMode = "-d" `elem` args
    inputContents <- getContents
    let input = lines inputContents
    solve input debugMode

solve input debugMode = do
    printList $ map digitsIndices $ input
    let values = map calibrationValue . map digitsIndices $ input
    if debugMode then printList values else return ()
    putStrLn $ show $ sum values

digitsIndices :: String -> [Int]
digitsIndices = go 0
    where
        go _ [] = []
        go index (thisChar:followingChars)
            | isDigit thisChar = index : rest
            | otherwise = rest
            where
                rest = go (index + 1) followingChars

calibrationValue :: [Int] -> Int
calibrationValue ints = 10 * (head ints) + (last ints)

printList :: Show a => [a] -> IO ()
printList = mapM_ putStrLn . map show

