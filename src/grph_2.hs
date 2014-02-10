import System.Environment (getArgs)
import qualified Data.HashTable.ST.Basic as H
import Control.Monad.ST (runST)
import Control.Monad (foldM)

main :: IO ()
main = do
    a <- getArgs >>= readFile . head
    mapM_ (\(x, y) -> putStrLn $ x ++ " " ++ y) $ grph 3 $ parse_fasta a

-- O(4 * N * O(h) + E) where O(h) is mostly O(1) hashtable lookups
grph :: Int -> [(String, String)] -> [(String, String)]
grph k x = runST $ do
    h <- H.new 
    foldM add h x >>= H.foldM mkResult []     
  where
    add h (name, strand) = do 
        let prefix = take k strand
            suffix = drop (length strand - k) strand
        add' suffix (\(a, b) -> (a, name:b)) ([], [name])
        add' prefix (\(a, b) -> (name:a, b)) ([name], [])
        return h
      where 
        add' s f1 f2 = H.lookup h s >>= H.insert h s . maybe f2 f1 
    mkResult r (_, (p, s)) = return $ [(sn, pn) | sn <- s, pn <- p, pn /= sn] ++ r
 
-- simple parser that doesn't check any errors                                                                                                              
parse_fasta :: String -> [(String, String)]
parse_fasta content = go $ lines content                                                                                                                    
  where                                                                                                                                                     
    go [] = []                                                                                                                                              
    go (id_line:xs) = let (body, xs') = break (\(x:_) -> x == '>') xs                                                                                       
                      in make_one (tail id_line) body : go xs'                                                                                              
    make_one id_line body = (head $ words id_line, concat body)    