import Text.Printf

main :: IO ()
main = do 
    l@(k:m:n:_) <- readInput
    let total = sum l
        l' = [(k, O_YY), (m, O_Yy), (n, O_yy)]
        iteration_1 = probs total l'
        iteration_2 = probs (pred total) [if i == i' then (c - 1, o) else x | i <- [0..2], (i', x@(c, o)) <- zip [0..2] l']
        final_probs = concat $ zipWith go (concat $ map (replicate 3) iteration_1) iteration_2
          where
            go (_, p1, o1) (_, p2, o2) = map (\(cp, o) -> (cp * p1 * p2, o)) $ crossObj o1 o2  
        r = foldl (\a (p, o) -> if o /= O_yy then a + p else a) 0.0 final_probs
    printf "%.5f\n" r
  where
    probs total = map (\(x, o) -> (x, fromInteger x / fromInteger total, o))  
    
readInput :: IO [Integer]
readInput = do
    putStrLn "Enter k m n:"
    l <- getLine
    let l' = map read $ words l
    if length l' /= 3 
        then putStrLn "Bad input" >> readInput  
        else return l'

data Obj = O_YY | O_Yy | O_yy deriving (Show, Eq)

crossObj :: Obj -> Obj -> [(Double, Obj)]
crossObj O_YY O_YY = [(1.0, O_YY)]
crossObj O_YY O_Yy = [(0.5, O_YY), (0.5, O_Yy)]
crossObj O_Yy O_YY = crossObj O_YY O_Yy 
crossObj O_YY O_yy = [(1.0, O_Yy)]
crossObj O_yy O_YY = crossObj O_YY O_yy
crossObj O_Yy O_Yy = [(0.25, O_YY), (0.5, O_Yy), (0.25, O_yy)]
crossObj O_Yy O_yy = [(0.5, O_Yy), (0.5, O_yy)]
crossObj O_yy O_Yy = crossObj O_Yy O_yy
crossObj O_yy O_yy = [(1.0, O_yy)]