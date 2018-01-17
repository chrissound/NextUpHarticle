{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Time.Calendar
import System.Environment
import Data.Aeson
import GHC.Generics
import Data.String.Conversions
import Data.Time.Format
import System.Directory

type TodoTitle = String
data Todo = Todo TodoTitle Day deriving (Generic)

instance FromJSON Todo
instance ToJSON Todo

instance Show Todo where
  show (Todo tt d) = show d ++ " - " ++ tt

todoSavePath :: FilePath
todoSavePath = "todoData.json"

saveTodos :: [Todo] -> IO ()
saveTodos t = do
  writeFile todoSavePath (convertString $ encode t)

todos :: IO [Todo]
todos = do
  fileExists <- doesFileExist todoSavePath
  if fileExists then do
    jsonEncoded <- readFile todoSavePath
    case (decode $ convertString jsonEncoded :: Maybe [Todo]) of
      Just x -> return x
      Nothing -> error "Failed parsing JSON"
  else return []


main :: IO ()
main = do
  putStrLn "NextUp!"
  args <- getArgs
  case args of
    ([]) -> todos >>= mapM_ print
    ("save":[]) -> error "Not able to parse correct amount of parameters"
    ("save":title:due:[]) -> do
      case (parseTimeM False defaultTimeLocale "%x" due :: Maybe Day) of
        Just day -> do
          let newTodo = Todo title day
          existingTodos <- todos
          saveTodos (newTodo : existingTodos)
          putStrLn $ "The following todo has been saved: \n" ++ show newTodo
        Nothing -> error "Failed parsing due date"
    _ -> error "Not able to parse correct amount of parameters"
