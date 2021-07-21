module Main where

import Data.List
import System.Environment


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


defuse :: Wires -> Bool -> IO String
defuse ws first
  | length [w | w <- ws, w > 0] >= 4 = return "SUCCESS"
  | otherwise   = do
      reportWires ws
      putStrLn "Cut a wire: "
      input <- getLine

      let color = colorFromString input
          valid = if first then True else checkCut ws color

      if valid
         then defuse (applyCut ws color) False
         else (return "BOOM!!!")


colorFromString :: String -> WireColor
colorFromString s =
  case s of
    "black" ->  Black
    "white" ->  White
    "orange" -> Orange
    "red" ->    Red
    "green" ->  Green
    "purple" -> Purple
    _        -> error (s ++ " does not correspond with a color")


reportWires :: Wires -> IO ()
reportWires ws = putStrLn $
  "Black  - " <> status (head ws) <> "\n" <>
  "White  - " <> status (ws!!1)   <> "\n" <>
  "Orange - " <> status (ws!!2)   <> "\n" <>
  "Red    - " <> status (ws!!3)   <> "\n" <>
  "Green  - " <> status (ws!!4)   <> "\n" <>
  "Purple - " <> status (ws!!5)   <> "\n"
  where
    status = \w -> if w > 0 then "Cut" else "Uncut"

main :: IO ()
main = do
  result <- defuse [0,0,0,0,0,0] True
  putStrLn result
