import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, find)
import Debug.Trace (trace)

main = interact $ formatOutput . map show . solveFor where
  solveFor input = [solvePartOne, solvePartTwo] <*> [input]
  formatOutput [!p1, !p2] = "Part 1: " <> p1 <> "\nPart 2: " <> p2 <> "\n"

solvePartOne = sum . map calibrationNumber . lines where
  calibrationNumber = read . firstAndLast . filter isDigit

firstAndLast items = [head, last] <*> [items]

debug x = trace (show x) x

solvePartTwo = sum . map calibrationNumber . lines where
  calibrationNumber = read . firstAndLast . findDigits
  findDigits = fst . foldr fd ([], "") where
    fd char (found, seen) = case readDigitFromStart curr of
      Nothing -> (found, curr)
      Just val -> (val:found, curr)
      where curr = char:seen
  readDigitFromStart s@(first:_)
    | isDigit first = Just first
    | otherwise     = find match spelledOut <&> fst where
      match (_digit, written) = s `startsWith` written
  startsWith = flip isPrefixOf
  spelledOut = zip ['0'..'9']
    [ "zero"
    , "one"
    , "two"
    , "three"
    , "four"
    , "five"
    , "six"
    , "seven"
    , "eight"
    , "nine"
    ]

