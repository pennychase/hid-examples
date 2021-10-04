import Data.Deque

main = do
    let st = push_front 15 $ push_front 10 $ push_front 5 $ push_front 0 empty
        -- st == 15, 10, 5, 0 
        st' = pop_front $ pop_front st -- 5, 0 
        st'' = push_front 100 st' -- 100, 5, 0 
        st''' = push_back 50 st' -- 5, 0, 50
        shouldBeTrue = [ front st' == Just 5,
                         front st'' == Just 100,
                         back st''' == Just 50,
                         isEmpty $ pop_front $ pop_front st',
                         isEmpty $ pop_back $ pop_back st'
                        ]
    print $ and shouldBeTrue