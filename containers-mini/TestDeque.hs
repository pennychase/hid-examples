import Data.Deque
import System.Exit (exitFailure)

main :: IO ()
main = do
    let d = push_front 15 $ push_front 10 $ push_front 5 $ push_front 0 empty
        -- d == 15, 10, 5, 0 <-- back
        d' = pop_front $ pop_front d -- 5, 0 <-- back
        d'' = push_front 100 d' -- 100, 5, 0 <-- back
        d''' = push_back 50 $ push_back 60 $ d' -- 5, 0, 50
        shouldBeTrue = [ front d' == Just 5,
                         front d'' == Just 100,
                         back d''' == Just 50,
                         isEmpty $ pop_front $ pop_front d',
                         isEmpty $ pop_back $ pop_back d'
                        ]
    case and shouldBeTrue of
        True -> pure ()
        False -> exitFailure