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


defuse :: Wires -> IO String
defuse ws
  | all (>0) ws = return "SUCCESS"
  | otherwise   = do
      putStrLn "Cut a wire: "
      input <- getLine

      let color = colorFromString input
          valid = checkCut ws color

      if valid
         then defuse (applyCut ws color)
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


main :: IO ()
main = do
  result <- defuse [0,0,0,0,0,0]
  putStrLn result
