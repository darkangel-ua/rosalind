import System.Environment(getArgs)
import Data.List(foldl')
import Text.Printf

main :: IO ()
main = do
    strand <- getArgs >>= read_line
    let Counters a c g t = dna strand
    printf "%d %d %d %d\n" a c g t
  where
    read_line (filename:_) = readFile filename >>= return . head . lines 
    read_line _ = error "one argument required"

data Counters = Counters !Int !Int !Int !Int
              
dna :: String -> Counters
dna = foldl' go (Counters 0 0 0 0) 
  where
    go (Counters a c g t) ch = 
        case ch of 
            'A' -> Counters (a + 1) c g t
            'C' -> Counters a (c +1) g t 
            'G' -> Counters a c (g + 1) t 
            'T' -> Counters a c g (t + 1)
            _   -> error $ "bad strand '" ++ [ch] ++ "'"
