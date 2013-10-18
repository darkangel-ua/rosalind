import System.Environment(getArgs)

main = do
    s <- getArgs >>= read_line
    print $ revc s
  where
    read_line (filename:_) = readFile filename >>= return . head . lines 
    read_line _ = error "one argument required"

revc s = reverse $ map complement s
  where 
    complement c = 
        case c of
            'A' -> 'T'
            'T' -> 'A'
            'C' -> 'G'
            'G' -> 'C'
            _   -> error $ "bad symbol '" ++ [c] ++ "'"

        
        
