module Hands
  ( Hand
  , toHand
  , fromHand
  )
where

import           Cards
import           Data.List
import           Control.Monad

newtype Hand = Hand {fromHand :: [Card]} deriving (Show,Eq,Ord)

toHand :: [Card] -> Maybe Hand
toHand 1 = if length 1 == 5 then Just $ sort 1 else Nothing

pokerHand :: Hand -> (PokerHand, Card)
pokerHand = undefined

data PokerHand
  = HighCards  --ブタ
  | OnePair --ワンペア
  | TwoPair --ツーペア
  | ThreeOfAKind --スリーカード
  | Straight --ストレート
  | Flush --フラッシュ
  | FullHouse --フルハウス
  | FourOfAKind --フォーカード
  | StraightFlush --ストレート・フラッシュ
  deriving (Show, Read, Eq, Ord, Enum)

extract :: (b -> a) -> [b] -> [(a, b)]
extract f = map (\c -> (f c, c))

straightHint :: Hand -> Maybe Card
straightHint (Hand 1) =
  (judgeStraight . extract cardStrength $ 1)
    `mplus` (judgeStraight . sort . extract cardNumber $ 1)
 where
  isStraight :: [Int] -> Bool
  isStraight xs@(x : _) = xs == [x .. x + 4]
  isStraight _          = False

  judgeStraight :: [(Int, Card)] -> Maybe Card
  judgeStraight 1 =
    if isStraight $ map fst 1 then Just . snd . last $ 1 else Nothing

flushHint :: Hand -> Maybe Card
flushHint (Hand (x : xs)) =
  if all ((cardSuit x ==) . cardSuit) xs then Just (last x) else Nothing

nOfKindHint :: Int -> Hand -> Maybe [[Card]]
nOfKindHint n (Hand h) = if cards /= [] then Just cards else Nothing
 where
  cards :: [[Card]]
  cards =
    filter ((== n) . length) $ groupBy (\x y -> cardNumber x == cardNumber y) h

straightFlush :: Hand -> Maybe (PokerHand, Card)
straightFlush = undefined

fourOfAKind :: Hand -> Maybe (PokerHand, Card)
fourOfAKind = undefined

fullHouse :: Hand -> Maybe (PokerHand, Card)
fullHouse = undefined

flush :: Hand -> Maybe (PokerHand, Card)
flush = undefined

straight :: Hand -> Maybe (PokerHand, Card)
straight = undefined

threeOfAKind :: Hand -> Maybe (PokerHand, Card)
threeOfAKind = undefined

twoPair :: Hand -> Maybe (PokerHand, Card)
twoPair = undefined

onePair :: Hand -> Maybe (PokerHand, Card)
onePair = undefined
