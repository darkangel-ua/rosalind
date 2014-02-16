import System.Environment(getArgs)
import Control.Monad (forM_)
import Data.List (tails, sortBy)

main :: IO ()
main = do
    c <- getArgs >>= readFile . head
    forM_ (revp . snd . head . parse_fasta $ c) $ 
        \(a, b) -> putStrLn $ show a ++ " " ++ show b

-- naive brute force algorithm
revp :: String -> [(Int, Int)]
revp strand = sortBy (\(lhs_pos, _) (rhs_pos, _) -> compare lhs_pos rhs_pos) 
                  $ concat 
                  $ [go k strand (complement strand) | k <- [4, 6..12]]
  where
    l = length strand
    go plen s cs = foldl f [] $ zip3 [1..] (mkSeq s) (mkSeq cs)
        where mkSeq s' = map (take plen) $ take (l - plen + 1) (tails s')
              f r (p, s', cs') = if (s' == reverse cs') then (p, plen) : r else r 
    
complement :: String -> String
complement s = map go s
  where 
    go c = 
        case c of
            'A' -> 'T'
            'T' -> 'A'
            'C' -> 'G'
            'G' -> 'C'
            _   -> error $ "bad symbol '" ++ [c] ++ "'"
    
-- simple parser that doesn't check any errors                                                                                                              
parse_fasta :: String -> [(String, String)]
parse_fasta content = go $ lines content                                                                                                                    
  where                                                                                                                                                     
    go [] = []                                                                                                                                              
    go (id_line:xs) = let (body, xs') = break (\(x:_) -> x == '>') xs                                                                                       
                      in make_one (tail id_line) body : go xs'                                                                                              
    make_one id_line body = (head $ words id_line, concat body)
        