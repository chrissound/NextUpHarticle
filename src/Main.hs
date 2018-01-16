module Main where

import Data.Time.Calendar

type TodoTitle = String
data Todo = Todo TodoTitle Day

main :: IO ()
main = putStrLn "NextUp!"
