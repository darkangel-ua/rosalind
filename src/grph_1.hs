import System.Environment (getArgs)
import Text.Printf
import Data.List (tails)

main :: IO ()
main = do
    a <- getArgs >>= readFile . head
    mapM_ (\(x, y) -> printf "%s %s\n" x y) $ grph (preprocess 3 $ parse_fasta a)

preprocess :: Int -> [(String, String)] -> [(String, String, String)] 
preprocess k = map (\(name, strand) -> (name, take k strand, drop (length strand - k) strand))

-- O(N^2/2)
grph :: [(String, String, String)] -> [(String, String)]
grph x = foldl go_i [] $ zip x j 
  where
    j = (init . tail . tails) x
    connected r (a_name, _, a_suffix) (b_name, b_prefix, _) = if a_suffix == b_prefix then (a_name, b_name) : r else r
    go_i r (a, j') = foldl go_j r j'
      where go_j r' b = connected (connected r' a b) b a  
 
-- simple parser that doesn't check any errors                                                                                                              
parse_fasta :: String -> [(String, String)]
parse_fasta content = go $ lines content                                                                                                                    
  where                                                                                                                                                     
    go [] = []                                                                                                                                              
    go (id_line:xs) = let (body, xs') = break (\(x:_) -> x == '>') xs                                                                                       
                      in make_one (tail id_line) body : go xs'                                                                                              
    make_one id_line body = (head $ words id_line, concat body)    