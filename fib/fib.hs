{-# LANGUAGE BangPatterns #-}

import System.IO

main = do
    n <- input "Enter n: "
    k <- input "Enter k: "
    print $ rfib_1 n k

input :: String -> IO Integer
input msg = do
    putStr msg >> hFlush stdout
    s <- getLine
    case reads s of
        [(x, "")] -> if x > 0 
                        then return x 
                        else putStrLn "Bad input: Enter positive integer" >> input msg
        _        -> putStrLn "bad input" >> input msg

-- direct but memory hungry
rfib n k = 
    let rfib' 1 = 1
        rfib' 2 = 1
        rfib' n'= rfib' (n' - 1) + k * rfib' (n' - 2) 
    in rfib' n

-- this is tail recursive with explicit state
rfib_1 n k = if n < 3 then 1 else go (n - 2) (1, 1)
  where
    go !n (!n1, !n2) 
       | n == 0 = n1 
       | otherwise = go (n - 1) (n1 + k * n2, n1) 
