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
    putStrLn $ case parseGames input of
        Left err -> "Error occurred: " ++ show err
        Right games -> fmtResult (solvePartOne games) (solvePartTwo games)

fmtResult :: Int -> Int -> String
fmtResult p1 p2 = "Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2

data Game = Game { gameId :: Int, sets :: [Set] }
    deriving Show

data Set = Set { r :: Int, g :: Int, b :: Int }
    deriving Show

solvePartOne :: [Game] -> Int
solvePartOne games = 0

solvePartTwo :: [Game] -> Int
solvePartTwo games = 0

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

asSet :: [Int] -> Set
asSet [r, g, b] = Set r g b
asSet _ = error "asSet: Expected a list of three integers"

quantityOf :: String -> [(String, Int)] -> Int
quantityOf color items = fromMaybe 0 $ snd <$> find (\(c, _) -> c == color) items

parseItem :: Parser (String, Int)
parseItem = do
    spaces
    quantity <- read <$> many digit
    spaces
    color <- many letter
    return (color, quantity)

printList :: Show a => [a] -> IO ()
printList = mapM_ putStrLn . map show
