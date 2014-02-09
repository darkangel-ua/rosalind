import System.Environment(getArgs)
import Data.List(maximumBy, foldl')

main :: IO ()
main = do
    s <- getArgs >>= read_file
    let (s_id, gc_content) = maximumBy (\(_, lhs) (_, rhs) -> compare lhs rhs) $ to_gc_content (parse_fasta s)
    putStrLn s_id
    print gc_content
  where
    read_file (filename:_) = readFile filename >>= return   
    read_file _ = error "one argument required"

-- simple parser that doesn't check any errors
parse_fasta :: String -> [(String, String)]
parse_fasta content = go $ lines content 
  where 
    go [] = []
    go (id_line:xs) = let (body, xs') = break (\(x:_) -> x == '>') xs
                      in make_one (tail id_line) body : go xs'
    make_one id_line body = (head $ words id_line, concat body)

to_gc_content = map $ \(sid, body) -> (sid, calc_gc body)
  where
    calc_gc body = let (l, gc) = foldl' (\(l, acc) x -> if x == 'C' || x == 'G' then (l + 1, acc + 1) else (l + 1, acc)) (0, 0) body
                   in fromIntegral gc / fromIntegral l * (100.0 :: Double)
    
