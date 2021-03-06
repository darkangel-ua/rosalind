import System.Environment(getArgs)

main :: IO ()
main = do
    strand <- getArgs >>= read_line
    print $ rna strand
  where
    read_line (filename:_) = readFile filename >>= return . head . lines 
    read_line _ = error "one argument required"

rna :: String -> String
rna = map $ \ch -> if ch == 'T' then 'U' else ch  
