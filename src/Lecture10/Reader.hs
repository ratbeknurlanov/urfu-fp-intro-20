module Lecture10.Reader where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Prelude hiding (id)

-- <Задачи для самостоятельного решения>

{-
  Задача: по имеющейся базе данных сфомировать набор рекламных писем.
  Для неженатого(-ой) персоны письмо должно иметь вид:

  Для мужчины:

"""
  Уважаемый Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Для женщины:

"""
  Уважаемая Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Семейным парам шлется одно письмо вида

"""
  Уважаемые Имя_мужа Отчество_мужа и Имя_жены Отчество_жены!
  Разрешите предложить вам наши услуги.
"""

-}

data Sex = Male | Female deriving (Show, Eq, Ord)

type PersonId = Int

data Person = Person
  { id :: Int
  , family :: String
  , name :: String
  , surname :: String
  , sex :: Sex
  , marriedBy :: Maybe Int
  } deriving (Show, Eq, Ord)

persons :: [Person]
persons =
  [ Person 1 "Иванов" "Иван" "Иванович" Male Nothing
  , Person 2 "Петров" "Петр" "Петрович" Male (Just 7)
  , Person 3 "Соловьева" "Алия" "Фаридовна" Female Nothing
  , Person 4 "Кузнецова" "Мария" "Ивановна" Female (Just 8)
  , Person 5 "Гринько" "Юлия" "Владимировна" Female Nothing
  , Person 6 "Кабанов" "Александр" "Романович" Male Nothing
  , Person 7 "Петрова" "Екатерина" "Алексеевна" Female (Just 2)
  , Person 8 "Кузнецов" "Евгений" "Семёнович" Male (Just 4)
  , Person 9 "Антонов" "Юрий" "Васильевич" Male Nothing
  ]

-- Поиск персоны по номеру
findById :: PersonId -> Reader [Person] (Maybe Person)
findById pId = do
  persons' <- ask
  return $ find (\p -> id p == pId) persons'

processSingle :: Person -> String
processSingle (Person _ _ name surname sex _) = case sex of
  Male -> "Уважаемый " ++ name ++ " " ++ surname ++ "!" ++ "\n" ++ "Разрешите предложить Вам наши услуги."
  Female -> "Уважаемая " ++ name ++ " " ++ surname ++ "!" ++ "\n" ++ "Разрешите предложить Вам наши услуги."

processPair :: Person -> Person -> String
processPair (Person husbandId _ husbandName husbandSurname _ (Just husbandMarriedBy)) (Person wifeId _ wifeName wifeSurname _ (Just wifeMarriedBy)) | husbandMarriedBy == wifeId && wifeMarriedBy == husbandId =
  "Уважаемые "++ husbandName ++ " " ++ husbandSurname ++ " и " ++ wifeName ++ " " ++ wifeSurname ++ "!" ++ "\n" ++ "Разрешите предложить вам наши услуги."
processPair _ _ = error "these persons is not married"

processPerson :: PersonId -> Reader [Person] (Maybe String)
processPerson pId = do
  person <- findById pId
  case person of
    Nothing -> return Nothing
    Just person' -> case marriedBy person' of
        Nothing -> return . Just $ processSingle person'
        Just spouseId -> do
            spouse <- findById spouseId
            return . Just $ case spouse of
                Nothing -> processSingle person'
                Just spouse' -> processPair person' spouse'

processPersons :: [PersonId] -> [Maybe String]
processPersons personIds = do
  personId <- personIds
  return $ runReader (processPerson personId) persons

-- </Задачи для самостоятельного решения>
