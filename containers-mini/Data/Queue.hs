{-# LANGUAGE NoImplicitPrelude #-}

module Data.Queue (
    Queue,
    empty,
    isEmpty,
    front,
    enqueue,
    dequeue) where

import Data.Bool (Bool)
import Data.Maybe (Maybe)
import Prelude (Show)
import Data.Deque hiding (empty, isEmpty, front)
import qualified Data.Deque as D (empty, isEmpty, front)

newtype Queue a = Queue (Deque a)
    deriving Show

empty :: Queue a
empty = Queue D.empty

isEmpty :: Queue a -> Bool
isEmpty (Queue q) = D.isEmpty q

front :: Queue a -> Maybe a
front (Queue q) = D.front q

enqueue :: a -> Queue a -> Queue a
enqueue e (Queue q) = Queue (push_back e q)

dequeue :: Queue a -> Queue a
dequeue (Queue q) = Queue (pop_front q)