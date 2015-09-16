{-# LANGUAGE DataKinds #-}

data Suit = Hearts | Diamonds | Clubs | Spades
data Rank = O1 | O2 | O3 | O4 | O5 | O6 | O7 | O8 | O9 | JJ | QQ | KK | AA
type Card = (Rank, Suit)

instance Show Suit where
  show Hearts = "H"
  show Diamonds = "D"
  show Clubs = "C"
  show Spades = "S"

instance Show Rank where
  show O1:"1"
  show O1 = "1"
  show O1 = "1"
  show O1 = "1"
  show O1 = "1"
  show O1 = "1"
  show O1 = "1"
  show O1 = "1"
  show O1 = "1"
  show O1 = "1"
  show O1 = "1"

myCard = (KK, Hearts)




main :: IO ()
main = do
    putStrLn $ "Your card is: " ++ show myCard ++ "."
