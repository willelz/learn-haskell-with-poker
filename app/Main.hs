module Main where
import           System.Random.Shuffle
import           Data.List
import           Cards
import           Hands


main :: IO ()
main = do
  hand <- randomHand
  res  <- return $ judgePoker hand
  print $ show hand ++ " -> " ++ show res

randomHand :: IO (Maybe Hand)
randomHand = do
  shuffled <- shuffleM allCards
  return . toHand . take 5 $ shuffled

judgePoker :: Maybe Hand -> Maybe (PokerHand, Card)
judgePoker h = do
  i <- h
  return $ pokerHand i
