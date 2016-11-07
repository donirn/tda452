
import Cards
-- Task 3.2 Size function description:
-- The signature of the function describes that from a hand we
-- obtain a number representing how many cards are stacked in the
-- Hand.
-- To calculate the function, the recursive nature of the hand definition is
-- considered.
-- An empty hand's size is 0. This is the base case for a hand.
-- size Empty            = 0
-- The only other possible case happens if the hand is not empty.
-- If that is the case, the structure of the hand consists of
-- a card and a hand that have been 'added'.
-- When this happens, we know that the size of that structure
-- is one (card) plus the size of the hand without that card.
-- This allows to pattern-match a type describing a card in order
-- to generate a sum of ones representing the size.

-- Task 3.3,
-- implement the function "empty",
empty :: Hand
empty = Empty
-- implement the function "value"
valueRank :: Rank -> Integer
valueRank (Numeric a) = a
valueRank Ace = 11
valueRank otherwise = 10

valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace suit) hand) = 1 + numberOfAces hand
numberOfAces (Add card hand) = numberOfAces hand

value :: Hand -> Integer
value Empty = 0
value (Add card hand) =
  let tempValue = value hand + valueCard card
  in if tempValue > 21
    then tempValue - (numberOfAces (Add card hand) * 10)
    else tempValue

-- implement the function "gameOver"
-- implement the function "winner".
card1 = Card King Spades -- 10
card2 = Card King Spades -- 11
card3 = Card (Numeric 7) Spades -- 4
hand1 = (Add card3 (Add card2 (Add card1 Empty)))
hand2 = (Add card3 (Add card2 (Add card1 Empty)))
