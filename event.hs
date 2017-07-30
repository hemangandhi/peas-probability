import Data.Either

data EventDependency = And   Event Event
                     | Or    Event Event
                     | Given Event Event
                     | Not   Event
                     | Raw   Fractional
                     deriving Show, Eq

data Event = Event {
    description :: [Char],
    depends :: EventDependency
} deriving Show, Eq

ifRawElseNothing :: (Fractional -> Fractional -> Fractional) -> (Event -> Event -> EventDependency) -> Event -> Event -> EventDependency
ifRawElseNothing fn _
                   (Event d_ev1 (Raw val_ev1))
                   (Event d_ev2 (Raw val_ev2)) = Raw $ fn val_ev1 val_ev2
ifRawElseNothing _ constructor ev1 ev2         = constructor ev1 ev2

getProbabilities :: Event -> Event
getProbabilities (Event d (Raw p))  = Event d $ Raw p
getProbabilities (Event d (Or l r))  = Event "computed" $
                                         ifRawElseNothing (\x y -> x + y - x * y)
                                                          Or
                                                          (getProbabilities l)
                                                          (getProbabilities r)
getProbabilities (Event d (And l r))  = Event "computed" $
                                         ifRawElseNothing (\x y -> x * y)
                                                          And
                                                          (getProbabilities l)
                                                          (getProbabilities r)
getProbabilities (Event d (Given l r))  = Event "computed" $
                                         ifRawElseNothing (\x y -> x * y)
                                                          Given
                                                          (getProbabilities l)
                                                          (getProbabilities r)

