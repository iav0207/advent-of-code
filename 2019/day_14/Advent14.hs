{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Advent14 where

import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, find)
import Debug.Trace (trace)
import Control.Applicative ()
import Text.Parsec ((<?>))
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P

debug x = trace (show x) x

main = interact $ formatOutput . map show . solveFor . parseInput where
  solveFor input = [solvePartOne, solvePartTwo] <*> [input]
  formatOutput [!p1, !p2] = "Part 1: " <> p1 <> "\nPart 2: " <> p2 <> "\n"

parseInput :: String -> [Reaction]
parseInput input = case P.parse reactions "" input of
  Left err -> error (show err)
  Right result -> debug result
  where
  reactions = reaction `P.sepEndBy` P.newline <* P.eof
  reaction = Reaction <$> (inps <* P.string " => ") <*> out
  inps = inp `P.sepBy` P.string ", "
  inp = ingredient
  out = ingredient
  ingredient = do
    qty <- read <$> P.many1 P.digit
    P.spaces
    material <- P.many1 P.letter
    return (qty, material)

data Reaction = Reaction { inp :: [QM], out :: QM }
  deriving (Show, Eq)
type QM = (Qty, Material)
type Qty = Int
type Material = String

-- DP
-- a state is Map Material Qty
-- an edge is a Reaction
-- greedy: the _less_ total quantity of materials on n-th step, the better

type State = Map.Map Material Qty

initState :: [Reaction] -> Map.Map Material Qty
initState = Map.fromList . map (,0) . concatMap allMaterials where
  allMaterials Reaction{..} = map snd (out:inp)

solvePartOne reacs = dp (initState reacs) where
  dp state = undefined

solvePartTwo _ = 0

