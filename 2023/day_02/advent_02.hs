import System.Environment
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = do
    args <- getArgs
    let debugMode = elem "-d" args
    input <- lines <$> getContents
    solve input debugMode

solve :: [String] -> Bool -> IO ()
solve input debugMode = do
    case parseGames input of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right games -> printList games

data Game = Game { gameId :: Int, sets :: [Set] }
    deriving Show

data Set = Set { r :: Int, g :: Int, b :: Int }
    deriving Show

asSet :: [Int] -> Set
asSet [r, g, b] = Set r g b
asSet _ = error "asSet: Expected a list of three integers"

parseGames :: [String] -> Either ParseError [Game]
parseGames lines = parse (many parseGame <* eof) "input lines" (unlines lines)

parseGame :: Parser Game
parseGame = do
    string "Game "
    gameId <- read <$> many digit
    string ": "
    sets <- sepBy parseSet (string "; ")
    spaces
    return $ Game gameId sets

parseSet :: Parser Set
parseSet = do
    items <- sepBy parseItem (string ", ")
    return $ asSet $ map (\c -> quantityOf c items) ["red", "green", "blue"]

parseItem :: Parser (String, Int)
parseItem = do
    spaces
    quantity <- read <$> many digit
    spaces
    color <- many letter
    return (color, quantity)

quantityOf :: String -> [(String, Int)] -> Int
quantityOf color items = case find (\(c, _) -> c == color) items of
    Just (_, qty) -> qty
    Nothing -> 0

printList :: Show a => [a] -> IO ()
printList = mapM_ putStrLn . map show
