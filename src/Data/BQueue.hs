-- boundable queue

module Data.BQueue where

import qualified Data.Sequence as S

-- fixme: kill debug
data BQ a = BQ Int (S.Seq a) deriving Show

empty :: Int -> BQ a
empty n = BQ n S.empty

singleton :: Int -> a -> BQ a
singleton n x = append x (empty n)

append :: a -> BQ a -> BQ a
append x (BQ n xs) = let s = xs S.|> x in
  BQ n (if S.length s > n then S.drop 1 s else s)

adjust :: (a -> a) -> Int -> BQ a -> BQ a
adjust f i (BQ n xs) = BQ n (S.adjust f i xs)

update :: Int -> a -> BQ a -> BQ a
update i x = adjust (const x) i
