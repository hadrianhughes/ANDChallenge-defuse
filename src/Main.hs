module Main where

import Data.List


data WireColor = Black | White | Orange | Red | Green | Purple deriving (Show, Eq)

-- [Black, White, Orange, Red, Green, Purple]
type Wires = [Int]


maximum' :: [Int] -> Int
maximum' [] = 0
maximum' (_:[]) = 0
maximum' (x:xs) = x `max` (maximum' xs)


applyCut :: Wires -> WireColor -> Wires
applyCut ws col =
  case col of
    Black ->  next:(tail ws)
    White ->  head ws:next:(drop 2 ws)
    Orange -> (take 2 ws) ++ (next:drop 3 ws)
    Red ->    (take 3 ws) ++ (next:drop 4 ws)
    Green ->  (take 4 ws) ++ (next:drop 5 ws)
    Purple -> (take 5 ws) ++ [next]
  where
    previous  = maximum' ws
    next = previous + 1


prevColor :: Wires -> WireColor
prevColor ws =
  case prevIndex of
    Just 0 -> Black
    Just 1 -> White
    Just 2 -> Orange
    Just 3 -> Red
    Just 4 -> Green
    Just 5 -> Purple
    Nothing -> error "No maximum element"
  where
    prevIndex = elemIndex (maximum' ws) ws


checkCut :: Wires -> WireColor -> Bool
checkCut ws c =
  case prev of
    Black ->  not $ c `elem` [White, Green, Orange]
    White ->  not $ c `elem` [White, Black]
    Orange -> c == Green
    Red ->    c `elem` [Orange, Black]
    Green ->  c `elem` [Orange, White]
    Purple -> c `elem` [Purple, Green, Orange, White]
  where
    prev = prevColor ws


defuse :: Wires -> [WireColor] -> Bool
defuse [] _ = True
defuse _ [] = True
defuse [0,0,0,0,0,0] (i:is) = defuse (applyCut [0,0,0,0,0,0] i) is
defuse ws (i:is)
  | checkCut ws i = defuse (applyCut ws i) is
  | otherwise     = False


main :: IO ()
main = putStrLn $ if defuse [0,0,0,0,0,0] [Red, Green, Purple, White] then "SUCCESS" else "BOOM!!"
