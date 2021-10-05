{-# LANGUAGE NoImplicitPrelude #-}

module Data.Deque (
    Deque,
    empty,
    isEmpty,
    front,
    back,
    push_front,
    push_back,
    pop_front,
    pop_back) where

import Data.Sequence hiding (empty)
import qualified Data.Sequence as Seq
import Data.Bool (Bool)
import Data.Maybe (Maybe (..))
import Prelude (Show)

newtype Deque a = Deque (Seq a)
    deriving Show
    
empty :: Deque a
empty = Deque Seq.empty

isEmpty :: Deque a -> Bool
isEmpty (Deque deq) = Seq.null deq

front :: Deque a -> Maybe a
front (Deque Seq.Empty) = Nothing
front (Deque (e :<| _)) = Just e

back :: Deque a -> Maybe a
back (Deque Seq.Empty) = Nothing
back (Deque (_ :|> e)) = Just e

push_front :: a -> Deque a -> Deque a
push_front e (Deque es) = Deque (e :<| es)

push_back :: a -> Deque a -> Deque a
push_back e (Deque es) = Deque (es :|> e)

pop_front :: Deque a -> Deque a
pop_front (Deque Seq.Empty) = Deque Seq.Empty
pop_front (Deque (_ :<| es)) = Deque es

pop_back :: Deque a -> Deque a
pop_back (Deque Seq.Empty) = Deque Seq.Empty
pop_back (Deque (es :|> _)) = Deque es