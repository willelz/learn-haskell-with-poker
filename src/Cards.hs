module Cards
  ( Suit(..)
  , Card
  , allCards
  , cardSuit
  , cardNumber
  , cardStrength
  , showCardNumber
  )
where

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Read, Eq,Ord,Enum)

data Card = Card Int Suit deriving (Eq, Ord)

instance Show Card where
  show (Card i Hearts  ) = "H" ++ showCardNumber i
  show (Card i Diamonds) = "D" ++ showCardNumber i
  show (Card i Clubs   ) = "C" ++ showCardNumber i
  show (Card i Spades  ) = "S" ++ showCardNumber i

cardSuit :: Card -> Suit
cardSuit (Card _ s) = s

cardNumber :: Card -> Int
cardNumber (Card n _) = n

cardStrength :: Card -> Int
cardStrength (Card n _) = n

showCardNumber :: Int -> String
showCardNumber 14 = "A_"
showCardNumber 13 = "K_"
showCardNumber 12 = "Q_"
showCardNumber 11 = "J_"
showCardNumber 10 = "10"
showCardNumber x  = show x ++ "_"

allCards :: [Card]
allCards = [ Card num suit | suit <- [Hearts ..], num <- [2 .. 14] ]
