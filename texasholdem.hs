{-# LANGUAGE DataKinds #-}

data Suit = Hearts | Diamonds | Clubs | Spades
data Rank = O1 | O2 | O3 | O4 | O5 | O6 | O7 | O8 | O9 | JJ | QQ | KK | AA
type Card = (Rank, Suit)

myCard = (KK, Hearts)

main :: IO ()
main = do
    putStrLn $ "Your card is: " ++ show myCard ++ "."
