main :: IO ()
main = do 
    (n:m:_) <- readInput
    print $ fibd n m

readInput :: IO [Integer]
readInput = do
    putStrLn "Enter 2 integers:"
    l <- getLine
    let l' = map read $ words l
    if length l' /= 2 
        then putStrLn "Bad input" >> readInput  
        else return l'

fibd :: Integer -> Integer -> Integer
fibd n m = go n $ 1 : replicate (fromInteger m - 1) 0
  where
    go 1 generations = sum generations
    go n' g = go (n' - 1) $ sum (tail g) : init g
    