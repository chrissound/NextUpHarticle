module Main where

import Data.Time.Calendar
import System.Environment

type TodoTitle = String
data Todo = Todo TodoTitle Day

instance Show Todo where
  show (Todo tt d) = show d ++ " - " ++ tt

saveTodos :: [Todo] -> IO ()
saveTodos _ = undefined

todos :: IO [Todo]
todos = return $ [
    Todo "Finish this tutorial" ( fromGregorian 2017 01 16 )
  , Todo "Talk dog for walk" ( fromGregorian 2017 01 16)
  , Todo "Sleep" ( fromGregorian 2017 01 16)
  ]

main :: IO ()
main = do
  putStrLn "NextUp!"
  args <- getArgs
  case args of
    ([]) -> todos >>= mapM_ print
    _ -> error "Not able to parse correct amount of parameters"


