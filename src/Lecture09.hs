{-# LANGUAGE DuplicateRecordFields #-}

module Lecture09 where

import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.IO (openFile, hGetLine, IOMode(..), hClose)
import System.Random
import Data.List (sortBy)

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show)

newtype Id = Id String deriving (Eq, Show)

newtype Title = Title String deriving (Eq, Show)

newtype Deadline = Deadline String deriving (Eq, Show)

newtype Content = Content String deriving (Eq, Show)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show)

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  createDirectoryIfMissing True rootFolder
  return (TodoList rootFolder)

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo (TodoList todoList) (Title title) (Content text) (Deadline deadline) = do
  let id = title ++ deadline
  let filename = (todoList ++ "\\" ++ id)
  writeFile filename (title ++ "\n" ++ show False ++ "\n" ++ deadline ++ "\n" ++ text ++ "\n")
  return (Id id)

readTodo :: TodoList -> Id -> IO Todo
readTodo (TodoList todoList) (Id id) =
  let
    parseStatus :: String -> Bool
    parseStatus status = case status of
      "True" -> True
      "False" -> False
      _ -> error "Invalid bool value in todo status"
  in
  do
  file <- openFile (todoList ++ "\\" ++ id) ReadMode
  title <- hGetLine file
  status <- hGetLine file
  deadline <- hGetLine file
  text <- hGetLine file
  hClose file
  return (Todo (Id id) (Title title) (Content text) (Deadline deadline) $ parseStatus status)

showTodo :: TodoList -> Id -> IO ()
showTodo (TodoList todoList) (Id id) = do
  todo <- readTodo (TodoList todoList) (Id id)
  showTodo' todo

showTodo' :: Todo -> IO ()
showTodo' (Todo (Id todoId) (Title title) (Content content) (Deadline deadline) isDone) =
  putStrLn (title ++ "\n" ++ show True ++ "\n" ++ deadline ++ "\n" ++ content ++ "\n")

removeTodo :: TodoList -> Id -> IO ()
removeTodo (TodoList todoList) (Id id) = do
  removeFile (todoList ++ "\\" ++ id)

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo (TodoList todoList) (Id id) (TodoEdit (Title updateTitle) (Content updateContent) (Deadline updateDeadline)) = do
  Todo _ _ _ _ isDone <- readTodo (TodoList todoList) (Id id)
  let filename = (todoList ++ "\\" ++ id)
  writeFile filename (updateTitle ++ "\n" ++ show isDone ++ "\n" ++ updateDeadline ++ "\n" ++ updateContent ++ "\n")

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone (TodoList todoList) (Id id) = do
  Todo (Id todoId) (Title title) (Content content) (Deadline deadline) _ <- readTodo (TodoList todoList) (Id id)
  let filename = (todoList ++ "\\" ++ id)
  writeFile filename (title ++ "\n" ++ show True ++ "\n" ++ deadline ++ "\n" ++ content ++ "\n")

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo (TodoList todoList) = do
  todoIds <- listDirectory todoList
  todos <- traverse (readTodo (TodoList todoList) . Id) todoIds
  return $ sortByDeadline todos

sortByDeadline :: [Todo] -> [Todo]
sortByDeadline = sortBy (\(Todo{deadline=(Deadline first)}) (Todo{deadline=(Deadline second)}) -> compare first second)

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
  allTodos <- readAllTodo todoList
  let filteredTodos = filterByIsDone allTodos False
  return filteredTodos

filterByIsDone :: [Todo] -> Bool -> [Todo]
filterByIsDone todos desiredStatus = filter (\t -> isDone t == desiredStatus) todos

showAllTodo :: TodoList -> IO ()
showAllTodo (TodoList todoList) = do
  todoIds <- listDirectory todoList
  mapM_ (showTodo (TodoList todoList) . Id) todoIds

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = do
  allTodos <- readAllTodo todoList
  let filteredTodos = filterByIsDone allTodos False
  mapM_ showTodo' filteredTodos

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37
  > Yep, that's the number!
-}

playGuessGame :: IO ()
playGuessGame = do
  number <- randomRIO (0, 100) :: IO Int
  run number

run :: Int -> IO ()
run number = do
  putStrLn "Your number: "
  guess <- getLine
  case compare (read guess) number of
    LT -> putStrLn "Too small" >> run number
    EQ -> putStrLn "Yep, that's the number!"
    GT -> putStrLn "Too big" >> run number

-- </Задачи для самостоятельного решения>