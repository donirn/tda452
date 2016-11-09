
import Cards
import RunGame
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
-- Function empty: Represents the base case for a hand.
empty :: Hand
empty = Empty

-- Function valueRank: Gives an numeric meaning for the different ranks.
-- From this function it is possible to construct other functions to
-- calculate the overall value of card collections.
valueRank :: Rank -> Integer
valueRank (Numeric a) = a
valueRank Ace         = 11
valueRank otherwise   = 10 -- Jack, Queen, King

-- Function valueCard: Extracts the value of a card based solely on the rank.
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

-- Function numberOfAces: Counts, in a recursive fashion, the number of aces
-- for a given hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand)
            | rank card == Ace = 1 + numberOfAces hand -- matches an Ace
            | otherwise        = numberOfAces hand -- any other kind of card.

-- Function value: Calculates the value of a hand based on the overall
-- bound of 21 that makes Ace value to have a higher value.
-- This is done by taking in consideration the accumulated value each time
-- whenever the base case is not the one to compute.
value :: Hand -> Integer
value Empty = 0
value (Add card hand) =
  let accumulatedValue = value hand + valueCard card
  in if accumulatedValue > 21
    -- Considers the value of an Ace as 1 i.e. substract 10 * number of aces.
    then accumulatedValue - (numberOfAces (Add card hand) * 10)
    else accumulatedValue

-- Function gameOver: Basic game rule.
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Function winner: Considers all the game rules as boolean conditions over
-- a pair of hands.
winner :: Hand -> Hand -> Player
winner guestHand bankHand | gameOver guestHand                = Bank
                          | gameOver bankHand                 = Guest
                          | value guestHand <= value bankHand = Bank
                          | otherwise                         = Guest
------------------------------------------------------------
-- Tests
card1 = Card (Numeric 3) Spades -- 10
card2 = Card King Spades -- 11
card3 = Card (Numeric 7) Spades -- 4
hand1 = (Add card3 (Add card2 (Add card1 Empty)))
hand2 = (Add card3 (Add card3 Empty))
