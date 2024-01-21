{-# LANGUAGE BangPatterns #-}
module Advent12 where

import Debug.Trace

debugMode = True
debug x = if debugMode then trace (show x) x else x

main = interact $ formatOutput . solve
formatOutput (!p1, !p2) = "Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2 ++ "\n"

solve input = (solvePartOne parsed, solvePartTwo parsed) where
  parsed = debug $ parseInput input

type Record = (String, [Int])

parseInput = map toRecord . lines where
  toRecord = wordsToRec . words
  wordsToRec [a, b] = (a, toInts b)
  toInts = map read . words . replace ',' ' '
  replace orig subst = map $ \c -> if c == orig then subst else c

solvePartOne !recs = sum $ map arrangements recs

arrangements = debug . onDot

onDot :: Record -> Integer
onDot (layout,  []) = if '#' `elem` layout then 0 else 1
onDot ([], signature) = 0
onDot (layout@(first:rest), signature) = ifDot + ifHash where
  ifDot = if first `elem` ".?" then onDot (rest, signature) else 0
  ifHash = if first `elem` "#?" then onHash (layout, signature) else 0

onHash :: Record -> Integer
onHash (layout, []) = 0
onHash (layout, group:restSign)
  | '.' `elem` take group layout = 0
  | length layout < group = 0
  | (take 1 . drop group) layout == "#" = 0
  | otherwise = onDot (drop (group + 1) layout, restSign)

solvePartTwo !input = "Not solved"

