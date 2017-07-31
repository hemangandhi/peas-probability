import System.Random.Shuffle
import System.Random
import Simulation

data Suit = Heart | Club | Diamond | Spade
          deriving (Show, Eq, Enum, Bounded, Ord)

data Value = Two | Three | Four | Five
           | Six | Seven | Eight | Nine | Ten
           | Jack | Queen | King | Ace
           deriving (Show, Eq, Enum, Bounded, Ord)

data Card = Card Value Suit
          deriving (Show, Eq)

mkDeck :: [Card]
mkDeck = [Card v s | s <- [Heart .. Spade],
                     v <- [Two .. Ace]]

mkDecks :: Int -> [Card]
mkDecks = concat . (flip replicate $ mkDeck)

nShuffledDecks :: [Card] -> Int -> IO [[Card]]
nShuffledDecks crds n = foldIOCards $ map (const (newStdGen >>= doShuffle)) [1..n]
    where doShuffle = return . shuffle' crds (length crds)
          foldIOCards = foldr (\new acc -> do cards <- new
                                              accum <- acc
                                              return $ accum ++ [cards])
                              (return [])

deal :: Int -> IO [[Card]] -> IO [[Card]]
deal n = (>>= return . map (take n))

getValue :: Card -> Value
getValue (Card v _) = v

hasNOfKind :: Int -> [Card] -> Bool
hasNOfKind n crds = n < (foldr max 0 $ getCountsOfThings $ map getValue crds)


main :: IO ()
main = (deal 12 $ nShuffledDecks (mkDecks 2) 1000) >>= return . simulateWithCounter events
                                                   >>= tryPrintWithProbability
    where events = [Event "two of a kind" (hasNOfKind 2),  Event "three of a kind" (hasNOfKind 3),
                    Event "four of a kind" (hasNOfKind 4), Event "five of a kind" (hasNOfKind 5)]
