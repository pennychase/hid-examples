import Data.List
import System.TimeIt

import Data.Deque as D
import Data.Stack as S


fill n insert s = foldl (flip insert) s [1..n]

sumAll s view remove = sum $ unfoldr iter s
    where 
        iter s = view s >>= \x -> Just (x, remove s)

main :: IO ()
main = do
    let n = 10^6
    timeItNamed "Stack" $
        print $ sumAll (fill n push S.empty) top pop
    timeItNamed "Deque" $
        print $ sumAll (fill n push_front D.empty) front pop_front