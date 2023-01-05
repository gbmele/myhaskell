module Zebra
  ( Resident(..)
  , Solution(..)
  , solve_zebra
  ) where
import Data.List (elemIndex, permutations)
import Data.Maybe (fromJust)
data Resident
  = Englishman
  | Spaniard
  | Ukrainian
  | Norwegian
  | Japanese
  deriving (Eq, Show)
data Drink
  = Coffee
  | Tea
  | Milk
  | OrangeJuice
  | Water
  deriving (Eq)
data Color
  = Red
  | Green
  | Ivory
  | Yellow
  | Blue
  deriving (Eq)
data Pet
  = Dog
  | Snails
  | Fox
  | Horse
  | Zebra
  deriving (Eq)
data Smoke
  = OldGold
  | Kools
  | Chesterfields
  | LuckyStrike
  | Parliaments
  deriving (Eq)
data Solution = Solution
  { waterDrinker :: Resident
  , zebraOwner :: Resident
  } deriving (Eq, Show)
solve_zebra :: Solution
solve_zebra =
  Solution
    { waterDrinker = nationalities !! fromJust (elemIndex Water drinks)
    , zebraOwner = nationalities !! fromJust (elemIndex Zebra pets)
    }
  where
    (nationalities, drinks, pets) = solution
solution :: ([Resident], [Drink], [Pet])
solution =
  head
    [ (nationalities, drinks, pets)
    | nationalities <-
        permutations [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
    , head nationalities == Norwegian
    , drinks <- permutations [Coffee, Tea, Milk, OrangeJuice, Water]
    , drinks !! 2 == Milk && isSame nationalities Ukrainian drinks Tea
    , colors <- permutations [Red, Green, Ivory, Yellow, Blue]
    , isRightOf colors Green colors Ivory &&
        isSame nationalities Englishman colors Red &&
        isNextTo nationalities Norwegian colors Blue &&
        isSame drinks Coffee colors Green
    , pets <- permutations [Dog, Snails, Fox, Horse, Zebra]
    , isSame nationalities Spaniard pets Dog
    , smokes <-
        permutations [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]
    , isSame smokes OldGold pets Snails &&
        isSame smokes Kools colors Yellow &&
        isNextTo smokes Chesterfields pets Fox &&
        isNextTo smokes Kools pets Horse &&
        isSame smokes LuckyStrike drinks OrangeJuice &&
        isSame nationalities Japanese smokes Parliaments
    ]
isSame :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> Bool
isSame xs x ys y = ys !! fromJust (elemIndex x xs) == y
isRightOf :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> Bool
isRightOf xs x ys y = index > 0 && ys !! (index - 1) == y
  where
    index = fromJust (elemIndex x xs)
isNextTo :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> Bool
isNextTo xs x ys y =
  index > 0 && ys !! (index - 1) == y ||
  index < (length ys - 1) && ys !! (index + 1) == y
  where
    index = fromJust (elemIndex x xs)