# Section 0 - Introduction

Right, so this is a first tutorial / guide / technical article I'm writing with Hart - <https://github.com/chrissound/Hart>. This means you will be able to clone down this project and follow along (and hack) at each section - seriously just do this:

```
git clone https://github.com/chrissound/NextUpHarticle
```

Within each section there will be two commits, the 'from' commit which is BEFORE the changes have occured relating to that section, and the `until` commit which is AFTER. So after cloning down the repo you can do a `git reset --HARD commitIdBEFORE` and follow along with the changes, or just `git reset --HARD commitIdAFTER` to see the end result of that section.

-----

Feedback is always appreciated. 

Right so what are we building here? We'll be building a simple command line based, todo list manager.

# Section 1 - First steps

```
Git From Commit: 
7f92f10dfe8e668d85d830ca6dcc1284d8cbec60

Git Until Commit: 
736d1dfefa4471b7eb43fbdc31834c99c0d13688
```

Right so we'll be storing the values as a JSON file. No we won't be using Hadoop, a database or some crazy quantum encryption device - let's be pragmatic.

First we need to add the time library to cabal: 

```
diff --git a/app.cabal b/app.cabal
index 4def916..1abd6ea 100644
--- a/app.cabal
+++ b/app.cabal
@@ -20,6 +20,7 @@ executable app
   -- other-modules:       
   -- other-extensions:    
   build-depends:       base >=4.10 && <4.11
+                     , time
   -- hs-source-dirs:      
   default-language:    Haskell2010
   hs-source-dirs: src

```

Lets define the types we know so far, and some imports.

```haskell
module Main where

import Data.Time.Calendar

type TodoTitle = String
data Todo = Todo TodoTitle Day

main :: IO ()
main = putStrLn "NextUp!"

```
# Section 2 - Input output 

```
Git From Commit: 
736d1dfefa4471b7eb43fbdc31834c99c0d13688

Git Until Commit: 
94cafa2979a2abb28e0d1aafcfb1a11bfd6c7ed8
```

Right lets add some functions relating to the functionality! We'll just mock this for now.

```haskell
diff --git a/src/Main.hs b/src/Main.hs
index e103e36..c42e702 100644
--- a/src/Main.hs
+++ b/src/Main.hs
@@ -1,9 +1,30 @@
 module Main where
 
 import Data.Time.Calendar
+import System.Environment
 
 type TodoTitle = String
 data Todo = Todo TodoTitle Day
 
+instance Show Todo where
+  show (Todo tt d) = show d ++ " - " ++ tt
+
+saveTodos :: [Todo] -> IO ()
+saveTodos _ = undefined
+
+todos :: IO [Todo]
+todos = return $ [
+    Todo "Finish this tutorial" ( fromGregorian 2017 01 16 )
+  , Todo "Talk dog for walk" ( fromGregorian 2017 01 16)
+  , Todo "Sleep" ( fromGregorian 2017 01 16)
+  ]
+
 main :: IO ()
-main = putStrLn "NextUp!"
+main = do
+  putStrLn "NextUp!"
+  args <- getArgs
+  case args of
+    ([]) -> todos >>= mapM_ print
+    _ -> error "Not able to parse correct amount of parameters"
+
+

```

Some potentially interesting things here:
`undefined` is essentially a function that returns an error, it's often used as a 'placeholder' to later on add an actual implementation. In Haskell you can't just have an 'empty' function - it has to return a value.
The `_` as the function parameter for `saveTodos` means we are ignoring this value (as we only plan to use this value later on).

# Section 3 - Jsonify

```
Git From Commit: 
94cafa2979a2abb28e0d1aafcfb1a11bfd6c7ed8

Git Until Commit: 
70369037a8a93a0c4aa66f8a1b539ea2a97f4fa1
```

Right, time to actually make this work!

Right lets add some functions relating to the functionality! We'll just mock this for now.

First we need some additional libraries, so we add them to cabal:

```haskell
diff --git a/app.cabal b/app.cabal
index 1abd6ea..0b07f9f 100644
--- a/app.cabal
+++ b/app.cabal
@@ -21,6 +21,9 @@ executable app
   -- other-extensions:    
   build-depends:       base >=4.10 && <4.11
                      , time
+                     , aeson
+                     , string-conversions
+                     , directory
   -- hs-source-dirs:      
   default-language:    Haskell2010
   hs-source-dirs: src

```

We add JSON encoding / decoding instance for our `Todo` data type, functionality to actually save and retrieve the files, and lastly we need add the ability to process a 'save' command.

```haskell
diff --git a/src/Main.hs b/src/Main.hs
index c42e702..d833564 100644
--- a/src/Main.hs
+++ b/src/Main.hs
@@ -1,23 +1,40 @@
+{-# LANGUAGE DeriveGeneric #-}
 module Main where
 
 import Data.Time.Calendar
 import System.Environment
+import Data.Aeson
+import GHC.Generics
+import Data.String.Conversions
+import Data.Time.Format
+import System.Directory
 
 type TodoTitle = String
-data Todo = Todo TodoTitle Day
+data Todo = Todo TodoTitle Day deriving (Generic)
+
+instance FromJSON Todo
+instance ToJSON Todo
 
 instance Show Todo where
   show (Todo tt d) = show d ++ " - " ++ tt
 
+todoSavePath :: FilePath
+todoSavePath = "todoData.json"
+
 saveTodos :: [Todo] -> IO ()
-saveTodos _ = undefined
+saveTodos t = do
+  writeFile todoSavePath (convertString $ encode t)
 
 todos :: IO [Todo]
-todos = return $ [
-    Todo "Finish this tutorial" ( fromGregorian 2017 01 16 )
-  , Todo "Talk dog for walk" ( fromGregorian 2017 01 16)
-  , Todo "Sleep" ( fromGregorian 2017 01 16)
-  ]
+todos = do
+  fileExists <- doesFileExist todoSavePath
+  if fileExists then do
+    jsonEncoded <- readFile todoSavePath
+    case (decode $ convertString jsonEncoded :: Maybe [Todo]) of
+      Just x -> return x
+      Nothing -> error "Failed parsing JSON"
+  else return []
+
 
 main :: IO ()
 main = do
@@ -25,6 +42,13 @@ main = do
   args <- getArgs
   case args of
     ([]) -> todos >>= mapM_ print
+    ("save":[]) -> error "Not able to parse correct amount of parameters"
+    ("save":title:due:[]) -> do
+      case (parseTimeM False defaultTimeLocale "%x" due :: Maybe Day) of
+        Just day -> do
+          let newTodo = Todo title day
+          existingTodos <- todos
+          saveTodos (newTodo : existingTodos)
+          putStrLn $ "The following todo has been saved: \n" ++ show newTodo
+        Nothing -> error "Failed parsing due date"
     _ -> error "Not able to parse correct amount of parameters"
-
-

```

Add now you should be able to:
```
stack exec app -- save "Testing todo" 01/01/18
```


And see output of:
```
NextUp!
The following todo has been saved: 
2018-01-01 - Testing todo

```

And to list all the todos:

```
stack exec app
```

And see output of:
```
NextUp!
2018-01-01 - Testing todo

```

Great! We've got the core functionality implemented!
