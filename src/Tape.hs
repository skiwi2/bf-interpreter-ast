module Tape
    ( Tape(..)
    , forwardTape
    , reverseTape
    , tapeValue
    ) where

data Tape a = Tape [a] a [a] deriving (Eq)

instance Show a => Show (Tape a) where
    show (Tape ls v rs) = show (reverse ls) ++ " " ++ show v ++ " " ++  show rs

forwardTape :: Tape a -> Tape a
forwardTape (Tape _ _ []) = error "Right side of tape is empty"
forwardTape (Tape ls v rs) = Tape (v:ls) (head rs) (tail rs)

reverseTape :: Tape a -> Tape a
reverseTape (Tape [] _ _) = error "Left side of tape is empty"
reverseTape (Tape ls v rs) = Tape (tail ls) (head ls) (v:rs)

tapeValue :: Tape a -> a
tapeValue (Tape _ v _) = v