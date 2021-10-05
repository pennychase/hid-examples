import Data.Queue
import System.Exit (exitFailure)

main :: IO ()
main = do
    let q = enqueue 15 $ enqueue 10 $ enqueue 5 $ enqueue 0 empty
        -- q == 0, 5, 10, 15 <<-- back
        q' = dequeue $ dequeue q -- q' == 10, 15 <<-- back
        q'' = enqueue 100 $ dequeue q' -- q'' == 15, 100 <<-- back
        shouldBeTrue = [ front q == Just 0,
                         front q' == Just 10,
                         front q'' == Just 15,
                         isEmpty $ dequeue $ dequeue q' ]
    case and shouldBeTrue of
        True -> pure ()
        False -> exitFailure