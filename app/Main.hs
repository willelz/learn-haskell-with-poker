module Main where
import           System.Random.Shuffle
import           Data.List
import           Safe
import           Data.Char
import           Cards
import           Hands


main :: IO ()
main = do
  putStrLn "------------------"
  putStrLn "-- simple poker --"
  putStrLn "------------------"
  deck <- shuffleM allCards

  case getHand deck of
    Nothing           -> error "予期せぬエラー"
    Just (hand, deck) -> playPoker hand deck
  ynQuestion "-- もっかいやる？" main (putStrLn "-- またねノシノシ")

playPoker :: Hand -> Deck -> IO ()
playPoker hand deck = do
  discards <- inputDisuse hand
  case drawHand deck discards hand of
    Nothing         -> error "予期せぬエラー"
    Just (nhand, _) -> do
      printHand [] nhand
      printResult $ pokerHand nhand

inputDisuse :: Hand -> IO DiscardList
inputDisuse hand = do
  printHand [] hand
  putStrLn "-- 捨てるカードを選んでね"
  gotDisuse <- getDiscardList hand
  case gotDisuse of
    Nothing -> do
      putStrLn "-- 1~5の数値を並べて入力してね"
      inputDisuse hand
    Just disuses -> do
      printHand disuses hand
      ynQuestion "-- これでいい？" (return disuses) (inputDisuse hand)

printResult :: (PokerHand, Card) -> IO ()
printResult (ph, card) = putStrLn
  $ concat ["***** あなたの手札は ", show ph, " で、最強カードは ", show card, " でした*****"]

printHand :: DiscardList -> Hand -> IO ()
printHand dis hand = putStrLn $ "-- 手札 : " ++ showChangeHand dis hand

ynQuestion :: String -> IO a -> IO a -> IO a
ynQuestion s yes no = do
  putStrLn $ s ++ "(y/n)"
  input <- getLine
  case input of
    "y" -> yes
    "n" -> no
    _   -> do
      putStrLn "-- `y`か`n`で入力してね"
      ynQuestion s yes no

showChangeHand :: DiscardList -> Hand -> String
showChangeHand dis h =
  let judge x =
        if elem x dis then " " ++ show x ++ " " else "[" ++ show x ++ "]"
  in  concat $ map judge (fromHand h)

randomHand :: IO (Maybe Hand)
randomHand = do
  shuffled <- shuffleM allCards
  return . toHand . take 5 $ shuffled

judgePoker :: Maybe Hand -> Maybe (PokerHand, Card)
judgePoker h = do
  i <- h
  return $ pokerHand i

type DiscardList = [Card] --捨て札
type Deck = [Card] --山札

drawHand :: Deck -> DiscardList -> Hand -> Maybe (Hand, Deck)
drawHand deck dis h =
  let nl = filter (flip notElem dis) (fromHand h)
      nr = drop (5 - length nl) deck
  in  (,) <$> toHand (take 5 $ nl ++ deck) <*> Just nr

getHand :: Deck -> Maybe (Hand, Deck)
getHand deck = do
  hand <- toHand . take 5 $ deck
  return (hand, drop 5 deck)

toIntList :: String -> Maybe [Int]
toIntList str = if and $ map isDigit str then Just $ reads str else Nothing
 where
  reads :: String -> [Int]
  reads = map $ read . (: [])

selectByIndexes :: [a] -> [Int] -> Maybe [a]
selectByIndexes l = sequence . map ((atMay l) . (subtract 1))

getDiscardList :: Hand -> IO (Maybe DiscardList)
getDiscardList h = do
  input <- getLine
  return $ do
    intList <- toIntList input
    res     <- selectByIndexes (fromHand h) intList
    return res
