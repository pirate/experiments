-- http://www.garrisonjensen.com/programming/2015/05/15/easy-red-black-tree.html
module RedBlackSet( empty
                  , member
                  , insert
                  ) where

data Tree a = Empty
            | T Color (Tree a) a (Tree a)

data Color  = R
            | B

empty :: Ord a => Tree a
empty = Empty

member :: Ord a => Tree a -> a -> Bool
member (T _ left e right) x | x == e = True
                            | x < e  = member left x
                            | x > e  = member right x
member Empty _                       = False

insert :: Ord a => a -> Tree a -> Tree a
insert x s = let T _ a y b = ins s
             in  T B a y b
        where
          ins s'@(T color a' y' b')
                    | x < y'    = build color (ins a') y' b'
                    | x > y'    = build color a' y' (ins b')
                    | otherwise = s'
          ins Empty             = T R Empty x Empty

build :: Color -> Tree a -> a -> Tree a -> Tree a
build B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
build B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
build B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
build B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
build color left x right          = T color left x right
