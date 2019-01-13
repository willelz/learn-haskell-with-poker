module Hands
  ( Hand
  , toHand
  , fromHand
  )
where

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
