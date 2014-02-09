main :: IO ()
main = do 
    i <- readInput
    print $ iev i

def :: [(Obj, Obj)]
def = [(O_YY, O_YY),
       (O_YY, O_Yy),
       (O_YY, O_yy),
       (O_Yy, O_Yy),
       (O_Yy, O_yy),
       (O_yy, O_yy) 
      ]

iev :: [Integer] -> Double
iev input_vector = 
    let probs = map (uncurry crossObj) def
        r_probs = concat $ zipWith go probs input_vector
            where go p n = map (\(po, o) -> (fromInteger n * 2.0 * po, o)) p
    in foldl (\a (po, o) -> if o /= O_yy then a + po else a) 0.0 r_probs
         
readInput :: IO [Integer]
readInput = do
    putStrLn "Enter 6 integers:"
    l <- getLine
    let l' = map read $ words l
    if length l' /= 6 
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