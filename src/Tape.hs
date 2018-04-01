module Tape
    ( Tape(..)
    , makeTape
    , forwardTape
    , reverseTape
    , tapeValue
    , onTapeValue
    ) where

data Tape a = Tape [a] a [a] deriving (Eq)

instance Show a => Show (Tape a) where
    show (Tape ls v rs) = show (reverse ls) ++ " " ++ show v ++ " " ++  show rs

makeTape :: a -> Tape a
makeTape def = Tape [] def []

forwardTape :: a -> Tape a -> Tape a
forwardTape def (Tape ls v [])  = Tape (v:ls) def []
forwardTape _ (Tape ls v rs)    = Tape (v:ls) (head rs) (tail rs)

reverseTape :: a -> Tape a -> Tape a
reverseTape def (Tape [] v rs)  = Tape [] def (v:rs)
reverseTape _ (Tape ls v rs)    = Tape (tail ls) (head ls) (v:rs)

tapeValue :: Tape a -> a
tapeValue (Tape _ v _) = v

onTapeValue :: (a -> a) -> Tape a -> Tape a
onTapeValue func (Tape ls v rs) = Tape ls (func v) rs