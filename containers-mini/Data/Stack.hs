module Data.Stack (
    Stack,
    empty,
    isEmpty,
    push,
    pop,
    top) where

newtype Stack a = Stack [a]
    deriving Show

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push e (Stack es) = Stack (e : es)

isEmpty :: Stack a -> Bool 
isEmpty (Stack es) = null es

pop :: Stack a -> Stack a
pop (Stack []) = empty
pop (Stack (_:es)) = Stack es
 
top :: Stack a -> Maybe a
top (Stack []) = Nothing 
top (Stack (e:_)) = Just e

