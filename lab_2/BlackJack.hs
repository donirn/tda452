module BlackJack where
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import System.Random

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

-- Calculating the size of a hand2 manually:
-- size hand2 =  size Add (Card (Numeric 2) Hearts)
--                     (Add (Card Jack Spades) Empty)
--              = 1 + size Add (Card Jack Spades) Empty
--              = 1 + 1 + size Empty
--              = 1 + 1 + 0
--              = 2

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
valueRank _           = 10 -- Jack, Queen, King

-- Function valueCard: Extracts the value of a card based solely on the rank.
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

-- Function numberOfAces: Counts, in a recursive fashion, the number of aces
-- for a given hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand -- Ace
numberOfAces (Add _ hand) = numberOfAces hand -- any other kind of card.

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

-- Function (<+) Puts the first hand on top of the second.
(<+) :: Hand -> Hand -> Hand
(<+) Empty scnHand = scnHand
(<+) fstHand Empty = fstHand
(<+) (Add card hand) scnHand = (Add card (hand <+ scnHand))

-- a QuickCheck property to check concatenation of 2 hands
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- TEST: Furthermore the size of the combined hand should be the sum of the sizes of the two individual hands:
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = size hand1 + size hand2 == size (hand1 <+ hand2)

suitDeck :: Suit -> Hand
suitDeck suit = foldr (<+) Empty hands
                where ranks =  [Jack, Queen, King, Ace] ++
                                [Numeric n | n<- [2..10]]
                      cards = [Card rank suit | rank <- ranks]
                      hands = [Add card Empty | card <- cards]

-- Function fullDeck
fullDeck :: Hand
fullDeck = suitDeck Hearts <+ suitDeck Spades <+ suitDeck Diamonds <+ suitDeck Clubs

-- Function draw: Given a deck and a hand, draw one card from the deck and
-- put on the hand. Return both the deck and the hand (in that order).
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, (Add card hand) )

-- Function playBank: Given a deck, play for the bank according to the rules
-- above (starting with an empty hand), and return the bank’s final hand:
playBank :: Hand -> Hand
playBank d = drawCard d Empty
  where drawCard deck hand | value hand >=16 = hand
                           | otherwise       = drawCard (fst t) (snd t) 
                                               where t = draw deck hand

shuffle :: StdGen -> Hand -> Hand
shuffle g deck = buildDeck Empty g deck

buildDeck :: Hand -> StdGen -> Hand -> Hand
buildDeck newDeck g d | size d == 0  = newDeck
                      | otherwise    = buildDeck (Add c' newDeck) g' d'
                                       where (n, g')  = randomNumber g d
                                             (c', d') = drawNthCard n d

randomNumber :: StdGen -> Hand -> (Integer, StdGen)
randomNumber g d = randomR (0, (size d) - 1) g


drawNthCard :: Integer -> Hand -> (Card, Hand)
drawNthCard n h | n < 0 || n >= size h = error "out of index"
drawNthCard n h = iterNthCard h n 0 Empty

iterNthCard :: Hand -> Integer -> Integer -> Hand -> (Card, Hand)
iterNthCard (Add c h) n i s | i == n    = (c, s <+ h)
                            | otherwise = iterNthCard h (i + 1) (Add c s)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty      = False
c `belongsTo` (Add c' h) = (c == c') || (c `belongsTo` h)

------------------------------------------------------------
-- Tests
card1 = Card (Numeric 3) Spades -- 10
card2 = Card King Spades -- 11
card3 = Card (Numeric 7) Spades -- 4
hand1 = (Add card3 (Add card2 (Add card1 Empty)))
hand2 = (Add card3 (Add card3 Empty))

cardTest = Card {rank = Numeric 2, suit = Spades}
handTest = Add (Card {rank = King, suit = Spades}) Empty
