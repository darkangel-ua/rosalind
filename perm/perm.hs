import System.IO
import Data.List(permutations)

main = do
    n <- input "Enter n: "
    let (c, p) = perm n
    print c
    mapM_ (\x -> print_list x >> putStrLn "") p
  where 
    print_list = mapM_ (\x -> putStr (show x) >> putStr " ")

input :: String -> IO Integer
input msg = do
    putStr msg >> hFlush stdout
    s <- getLine
    case reads s of
        [(x, "")] -> if x > 0 
                        then return x 
                        else putStrLn "Bad input: Enter positive integer" >> input msg
        _        -> putStrLn "bad input" >> input msg

perm n = let a = [1..n] 
             p = permutations a
         in (length p, p) 
