import System.Environment
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (liftA2)

main :: IO ()
main = interact $ liftA2 fmtResult solvePartOne solvePartTwo . parseGames . lines

fmtResult :: Int -> Int -> String
fmtResult p1 p2 = "Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2

data Game = Game { gameId :: Int, sets :: [Set] } deriving Show
data Set = Set { r :: Int, g :: Int, b :: Int } deriving Show
type Bag = Set

bagOf [r, g, b] = Set r g b
cubes (Set r g b) = [r, g, b]

solvePartOne :: [Game] -> Int
solvePartOne games = sum $ map gameId $ filter (isPossibleWith bag) games where
    bag = Set 12 13 14
    isPossibleWith bag (Game { sets = sets }) = all (fitsIn bag) sets
    fitsIn bag set = and $ zipWith (<=) (cubes set) (cubes bag)

solvePartTwo :: [Game] -> Int
solvePartTwo = sum . map (power . minimalBag) where
    power = product . cubes
    minimalBag game = foldl coerceAtLeast (bagOf [0, 0, 0]) (sets game)
    coerceAtLeast bagA bagB = bagOf $ zipWith max (cubes bagA) (cubes bagB)

parseGames :: [String] -> [Game]
parseGames lines = case parse (many parseGame <* eof) "" (unlines lines) of
    Left err -> error (show err)
    Right games -> games

parseGame :: Parser Game
parseGame = do
    string "Game "
    gameId <- read <$> many digit
    string ": "
    sets <- parseSet `sepBy`  string "; "
    spaces
    return $ Game gameId sets

parseSet :: Parser Set
parseSet = do
    items <- sepBy parseItem (string ", ")
    return $ asSet $ map (`quantityOf` items) ["red", "green", "blue"]

asSet :: [Int] -> Set
asSet [r, g, b] = Set r g b
asSet _ = error "asSet: Expected a list of three integers"

quantityOf :: String -> [(String, Int)] -> Int
quantityOf color items = maybe 0 snd $ find (\(c, _) -> c == color) items

parseItem :: Parser (String, Int)
parseItem = do
    spaces
    quantity <- read <$> many digit
    spaces
    color <- many letter
    return (color, quantity)

printList :: Show a => [a] -> IO ()
printList = mapM_ print
