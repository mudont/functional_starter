{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Debug.Trace
import System.IO

data Mark = O | X deriving (Show, Eq)

type Row = [Maybe Mark]

type Board = [Row]

data TTTState = TTTState {_board :: !Board, _turn :: !Mark} deriving (Show)

makeLenses ''TTTState

emptyRow :: Row
emptyRow = replicate 3 Nothing

emptyBoard :: Board
emptyBoard = replicate 3 emptyRow

showMark :: Maybe Mark -> String
showMark (Just m) = case m of
  O -> "O"
  X -> "X"
showMark Nothing = "_"

printRow :: Row -> IO ()
printRow [a, b, c] =
  putStrLn $ showMark a <> showMark b <> showMark c

ppTT :: Board -> IO ()
ppTT [r1, r2, r3] = do
  printRow r1
  printRow r2
  printRow r3

isEmpty :: Int -> Int -> TTTState -> Bool
isEmpty r c st =
  case currVal of
    Just Nothing -> True
    _ -> False
  where
    currVal = st ^? lens
    lens = board . ix (r - 1) . ix (c - 1)

updateMove :: Mark -> Int -> Int -> TTTState -> TTTState
updateMove mark row col st =
  if validUpdate then newSt else st
  where
    validUpdate = isEmpty row col st
    lens = board . ix (row - 1) . ix (col - 1)
    newSt = flipTurn (st ^. turn) $ set lens (Just mark) st

flipTurn :: Mark -> TTTState -> TTTState
flipTurn t =
  set turn t'
  where
    t' = if t == O then X else O

n :: Int
n = 3

getWinSeqs :: Board -> [Row]
getWinSeqs b =
  rows ++ cols ++ mainDiag ++ otherDiag
  where
    rows = b
    cols = transpose b
    mainDiag = [zipWith (!!) b [0 ..]]
    otherDiag = [zipWith (!!) b [n - 1, n - 2 ..]]

getRowWinner :: Row -> Maybe Mark
getRowWinner [a, b, c] =
  if a == b && b == c && isJust a
    then a
    else Nothing

finished :: Board -> IO Bool
finished b = do
  case find isJust $ map getRowWinner (getWinSeqs b) of
    Just (Just x) -> do
      print $ show x <> " Won"
      return True
    _ -> return False

play :: StateT TTTState IO ()
play = do
  let loop = do
        st <- get
        liftIO $ putStr $ "Enter " <> show (st ^. turn) <> " move <i j>: "

        locs <- liftIO $ fmap (map (read :: String -> Int) . words) getLine
        liftIO $ putStrLn ""

        let st2 = updateMove (st ^. turn) (locs !! 0) (locs !! 1) st
        put st2

        liftIO $ ppTT $ st2 ^. board
        gameOver <- lift $ finished (st2 ^. board)
        unless gameOver loop
  loop

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (_, st) <- runStateT play $ TTTState emptyBoard X
  putStrLn "Game Over!"
