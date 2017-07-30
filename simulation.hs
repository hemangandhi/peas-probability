module Simulation (Event (Event, And, Or, Given, Not),
                   Simulation,
                   foldOnSimulation,
                   mkEmptySimulation,
                   getCountsOfThings) where


import qualified Data.Map as Map

data Event s = Event [Char] (s -> Bool)
             | And   (Event s) (Event s)
             | Or    (Event s) (Event s)
             | Given (Event s) (Event s)
             | Not   (Event s)

instance Show (Event t) where
    show (Event s _) = s
    show (Not ev)    = "Not " ++ show ev
    show (And l r)   = show l ++ " and " ++ show r
    show (Or l r)    = show l ++ " or " ++ show r
    show (Given l r) = show l ++ " given " ++ show r

--I hate this, but ease...
instance Eq (Event t) where
    ev1 == ev2 = (show ev1) == (show ev2)

instance Ord (Event t) where
    compare ev1 ev2 = compare (show ev1) (show ev2)

type Simulation t = Map.Map (Event t) Int

isEventFired :: Event t -> t -> Bool
isEventFired (Event _ fn) t = fn t
isEventFired (Not ev)     t = not $ isEventFired ev t
isEventFired (And l r)    t = and [isEventFired l t, isEventFired r t]
isEventFired (Or l r)     t = or [isEventFired l t, isEventFired r t]
isEventFired (Given l r)  t = and [isEventFired l t, isEventFired r t]

foldOnSimulation :: t -> Simulation t -> Simulation t
foldOnSimulation new = Map.mapWithKey addIfFired
    where addIfFired ev ct = if   isEventFired ev new
                             then ct + 1
                             else ct

mkEmptySimulation :: [Event t] -> Simulation t
mkEmptySimulation = foldr (flip Map.insert $ 0) Map.empty

getCountsOfThings :: Ord t => [t] -> Map.Map t Int
getCountsOfThings = foldr doInsert Map.empty
    where doInsert val accMap = Map.insertWith (+) val 1 accMap
