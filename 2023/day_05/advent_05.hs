import Control.Applicative (liftA2)
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text as T
import System.Environment
import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = interact $ (++"\n") . show . parseGiven

data Given      = Given { seeds :: [Integer], categories :: [Category] }    deriving Show
data Category   = Category { name :: String, rest :: String } deriving Show
-- data Category   = Category { name :: String, ranges :: [Range] }              deriving Show
-- data Range      = Range { startInc :: Integer, endExc :: Integer }          deriving Show

parseGiven :: String -> Given
parseGiven content = case parse given "" content of
    Left err -> error (show err)
    Right result -> result
  where
    given = Given <$> (string "seeds: " *> int `sepBy` char ' ') <*> (string "\n\n" *> category `sepBy` string "\n\n")
    category = Category <$> (catName  <* char ':') <*> tillCatEnd
    tillCatEnd = manyTill anyChar (try (string "\n\n"))
    catName = many (noneOf ":\n")
    int = read <$> many1 digit

