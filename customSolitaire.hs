data Color = Red | Black
    deriving (Eq, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Eq, Show)

data Rank = Num Int | Jack | Queen | King | Ace
    deriving (Eq, Show)

data Card = Card { suit :: Suit, rank :: Rank }
    deriving (Eq, Show)

data Move = Draw | Discard Card
    deriving (Eq, Show)

-- Question 1
cardColor :: Card -> Color
cardColor (Card {suit = suit, rank = _ })
    | suit == Hearts = Red
    | suit == Diamonds = Red
    | otherwise = Black

-- Question 2
cardValue :: Card -> Int
cardValue (Card {suit = _, rank = rank })
    | rank == Jack = 10
    | rank == Queen = 10
    | rank == King = 10
    | rank == Ace = 11
    | otherwise = r
        where Num r = rank

-- Question 3
isSameCard :: Card -> Card-> Bool
isSameCard (Card {suit = suite1, rank = rank1 }) (Card {suit = suite2, rank = rank2 })
    | suite1 == suite2 && rank1 == rank2 = True
    | otherwise = False

removeCard :: Card -> [Card] -> [Card]
removeCard x (y:ys) | isSameCard x y  = ys
                    | otherwise = y : removeCard x ys

-- Question 4
allSameColor :: [Card] -> Bool
allSameColor (x1:x2:xs) | cardColor x1 /= cardColor x2 = False
                        | cardColor x1 == cardColor x2 && xs == [] = True
                        | otherwise = allSameColor (x2:xs)


-- Question 5
sumCards :: [Card] -> Int
sumCards y = sumCards' y 0 where
    sumCards' (x:xs) t
        | xs == [] = (t + cardValue x)
        | otherwise = sumCards' xs (t + cardValue x)

-- Question 6
score :: [Card] -> Int -> Int
score x goal
    | allSameColor x == True =  floor (fromIntegral (preliminaryScore x goal) / 2.0)
    | otherwise = preliminaryScore x goal
        where preliminaryScore x goal |sumCards x > goal = 3 * (sumCards x - goal)
                                        | otherwise = goal - sumCards x
