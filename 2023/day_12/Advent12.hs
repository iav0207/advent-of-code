{-# LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace
import Data.List
import Data.Function.Memoize (memoize)

debugMode = False
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
solvePartTwo !recs = sum $ map arrangements extended where
  extended = map extend recs
  extend (layout, signature) = ((intercalate "?" . replicate 5) layout, (flatten . replicate 5) signature)
  flatten = foldr (<>) []

arrangements = debug . onDot

-- FIXME: this memoization doesn't work, the program hangs

onDot :: Record -> Integer
onDot = memoize od where
  od (layout,  []) = if '#' `elem` layout then 0 else 1
  od ([], signature) = 0
  od (layout@(first:rest), signature) = ifDot + ifHash where
      ifDot = if first `elem` ".?" then onDot (rest, signature) else 0
      ifHash = if first `elem` "#?" then onHash (layout, signature) else 0

onHash :: Record -> Integer
onHash = memoize oh where
  oh (layout, []) = 0
  oh (layout, group:restSign)
      | '.' `elem` take group layout = 0
      | length layout < group = 0
      | (take 1 . drop group) layout == "#" = 0
      | otherwise = onDot (drop (group + 1) layout, restSign)

