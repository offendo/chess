module Main where

import Board

main :: IO ()
main = putStrLn $ showBoard initBoard
