module MyModule (
    Point(..)
    , Shape(..)
    , surface
    , nudge
    , baseCircle
    , baseRect
    ) where

import qualified Data.Map as Map

-- data Shape = Circle Float Float Float 
--     | Rectangle Float Float Float Float deriving (Show)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1) 

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b)) 

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- Record type:
-- data Person = Person {firstName :: String
--                     , lastName :: String
--                     , age :: Int
--                     , height :: Float
--                     , phoneNumber :: String
--                     , flavor :: String
--                     } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n


-- Derived instances
data Person = Person {firstName :: String
                    , lastName :: String
                    , age :: Int} deriving (Eq, Show, Read)

mikeD = Person {firstName = "Micheal", lastName = "Diamond", age = 43}
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

-- mca == adRock -> False
-- mikeD == adRock -> False
-- mikeD == mikeD -> True

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)


-- Type synonyms:
-- type String = [Char]

type IntMap v = Map.Map Int v
-- or: type IntMap = Map Int

-- type Either a b = Left a | Right b 
--     deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookUp :: Int -> LockerMap -> Either String Code
lockerLookUp lockerNumber map = 
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!" 

        
lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  


-- Recursive data structures
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- the same as in records syntax:
{-
data List a = Empty | Cons {listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
-}

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- typeclass:
{- this is how Eq defined in standard prelude:

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x \= y)
    x \= y = not (x == y)
-}

-- it defines the states of a traffic light:
data TrafficLight = Red | Yellow | Green

-- this is how we make it an instance of Eq:
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- this is how we make it an instance of Show:
instance Show TrafficLight where 
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- you can also make typeclass that are subclassed of other typeclass
{-
class (Eq a) => Num a where
    ...
-}

-- Notice that "Maybe" is not a concrete type
-- so it's we can't do something like this:
{-
instance Eq Maybe where
    ...
-}

-- because that "a" has to be a concrete type
-- so we could write it out like so:
{-
instance Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
-}


class YesNo a where 
    yesno :: a -> Bool

instance YesNo Int where 
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where 
    yesno [] = False
    yesno _ = True

instance YesNo Bool where 
    yesno = id

instance YesNo (Maybe a) where 
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where 
    yesno EmptyTree = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- The Functor typeclass:
{-
class Functor f where 
    fmap :: (a -> b) -> f a -> f b

-- a quick refresher, the type signature of map is:
map :: (a -> b) -> [a] -> [b]

-- in fact, map is just a fmap that works on list!
instance Functor [] where
    fmap = map

-- since "f" has to be a type constructor that takes one type,
-- [a] is already a concrete type, so we didn't write:
-- instance Functor [a] where ...


-- here is how Maybe can be a functor:
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
-}

-- another thing that can be mapped over is our "Tree a" type:
instance Functor Tree where 
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- The Functor typeclass wants a type constructor that takes 
-- only one type parameter, but Either a b has two
-- we can partially feeding only one parameter so that it has only one free parameter
{-
instance Functor (Either a) where 
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
-}
