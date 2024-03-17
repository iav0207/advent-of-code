module Main where

import Text.Parsec
import Debug.Trace

debug x = traceShow x x

main = interact $ formatOutput . map show . solveFor where
  formatOutput [!p1, !p2] = "Part 1: " <> p1 <> "\nPart 2: " <> p2 <> "\n"
  solveFor input = [solvePart 1, solvePart 2] <*> [input]

solvePart 1 input = zero $ debug $ parseInput input where
  zero !v = const 0 v

solvePart 2 _ = 0

dimens = "xmas"

parseInput :: String -> ([Workflow], [Point])
parseInput input = case parse root "input" input of
  Left err -> error $ show err
  Right val -> val
  where
  root = do
    wfs <- workflows
    _ <- string "\n"
    ps <- points
    return (wfs, ps)

  workflows = workflow `sepEndBy` newline
  workflow = do
    wfName <- many1 letter
    _ <- char '{'
    branches <- branch `sepBy` char ','
    _ <- char '}'
    return (wfName, branches)
  branch = many1 $ noneOf ",}"

  points :: Parsec String () [[Int]]
  points = (char '{' *> point <* char '}') `sepEndBy` newline
  point = coord `sepBy` char ','
  coord = (anyChar >> anyChar) *> int

  int = read <$> many1 digit

type Workflow = (Name, [Branch])
type Name = String
type Branch = String
type Point = [Int]

