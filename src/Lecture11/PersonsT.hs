{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture11.PersonsT where

import Data.List
import Data.Maybe
import Data.Functor
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (id)
import Lecture10.Reader (Person (..), PersonId, persons, processSingle, processPair)


-- <Задачи для самостоятельного решения>

{-
  В этом задании нужно адаптировать код из 10 лекции к новым требованиям
  при помощи трансформеров.

  1. Необходимо собирать статистику найденных людей:
    - количество одиноких персон
    - количество замужних персон
  2. В функцию `findById` добавить логгирование с помощью монады Write.
    Нужно сообщать была ли найдена персона по данному id или нет.

  Вы можете переиспользовать функции, которые остались без изменения из Lecture10.Reader
-}

data PersonSearchStats = PersonSearchStats
  { marriedPersonsCount :: Integer
  , singlePersonsCount :: Integer
  } deriving (Show)

emptyStats :: PersonSearchStats
emptyStats = PersonSearchStats 0 0

newtype PersonsT a = PersonsT
  { runPersonsT :: (ReaderT [Person] (StateT PersonSearchStats (Writer [String])) a) }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState PersonSearchStats
    , MonadReader [Person]
    , MonadWriter [String]
    )

runPersons :: PersonsT a -> ((a, PersonSearchStats), [String])
runPersons = runWriter . flip runStateT emptyStats . flip runReaderT persons . runPersonsT

findById :: PersonId -> PersonsT (Maybe Person)
findById pId = do
  persons' <- ask
  let found = find (\p -> id p == pId) persons'
  case found of
    Just person -> tell ["Found: " ++ show (id person)]
    _ -> tell ["Not found: " ++ show pId]

  return $ found

processPerson :: PersonId -> PersonsT (Maybe String)
processPerson pId = do
  put emptyStats
  person <- findById pId
  let sId = join $ marriedBy <$> person
  spouse <- case sId of
    (Just sId) -> findById sId
    _ -> return Nothing
  case (person, spouse) of
    (Nothing, _) -> return Nothing
    (person, Nothing) -> modify addSingle $> liftM processSingle person
    (person, spouse) -> modify addMarried $> liftM2 processPair person spouse
  where
    addSingle stats@(PersonSearchStats _ s) = stats {singlePersonsCount = s + 1}
    addMarried stats@(PersonSearchStats m _) = stats {marriedPersonsCount = m + 1}

{-
  Функция должна выводить на экран:
  - общую поисковую статистику по всем найденым персонам.
  - результат каждого поиска

  Записывать в "persons.log" общий лог всех поисков.
-}
processPersons :: [PersonId] -> IO ()
processPersons personIds = do
  let ((results, stats), logs) = runPersons $ mapM (\p -> do
                                                        processed <- processPerson p
                                                        return $ (p, processed)) personIds
  mapM_ (\(i, res) -> putStrLn ("Found: " ++ show res ++ ";" ++  "id: " ++ show i)) results
  putStrLn ("Total: " ++ "\n" ++ show stats)
  writeFile "persons.log" (show logs)

-- </Задачи для самостоятельного решения>
