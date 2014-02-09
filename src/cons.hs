import System.Environment(getArgs)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad (foldM, liftM)
import Control.Monad.ST (runST)

-- TODO: rewrite to use ByteStrings and Array-s
(|>) :: a -> (a -> b) -> b
(|>) x y = y x

type Matrix = V.Vector (UV.Vector Int)

main :: IO ()
main = do
    c <- getArgs >>= readFile . head
    let strings = parse_fasta c |> map snd
        m = mkMatrix strings 
        consensus = mkConsensus m
    putStrLn consensus
    printMatrix m   

printMatrix :: Matrix -> IO ()
printMatrix m = let prn i = putStr (decode i : ": ") >> (mapM_ (\x -> putStr $ show x ++ " ") $ UV.toList $ (V.!) m i) >> putStrLn "" 
                in mapM_ prn [0..3] 
     
mkMatrix :: [String] -> Matrix
mkMatrix strings = runST $ do
    let string_len = head strings |> length 
    init_state <- MV.replicate string_len 0 |> V.replicateM 4  
    r <- foldM (\m s -> foldM inc m (zip s [0..])) init_state strings
    V.toList r |> mapM UV.unsafeFreeze |> liftM V.fromList
  where
    inc m (c, pos) = do
        let v = (V.!) m (encode c)
        MV.unsafeRead v pos >>= (MV.unsafeWrite v pos) . (+1)
        return m 

mkConsensus :: Matrix -> String
mkConsensus m = map go [0.. UV.length ((V.!) m 0) - 1]
  where
    go i = decode . fst . maximum $ [(j, at i j) | j <- [0..3]]
    at x y = (UV.!) ((V.!) m y) x    
    
encode :: Char -> Int
encode c 
    | c == 'A' = 0
    | c == 'C' = 1
    | c == 'G' = 2
    | c == 'T' = 3
    | otherwise = error "encode:bad input"    

decode :: Int -> Char
decode i = (UV.!) (UV.fromList "ACGT") i
    
-- simple parser that doesn't check any errors                                                                                                              
parse_fasta :: String -> [(String, String)]
parse_fasta content = go $ lines content                                                                                                                    
  where                                                                                                                                                     
    go [] = []                                                                                                                                              
    go (id_line:xs) = let (body, xs') = break (\(x:_) -> x == '>') xs                                                                                       
                      in make_one (tail id_line) body : go xs'                                                                                              
    make_one id_line body = (head $ words id_line, concat body)
    