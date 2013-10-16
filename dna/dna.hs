import System.Environment(getArgs)
import Text.Printf

main = do
    strand <- getArgs >>= read_line
    let Counters a c g t = dna strand
    printf "%d %d %d %d\n" a c g t
  where
    read_line (filename:_) = readFile filename >>= return . head . lines 
    read_line _ = error "one argument required"

data Counters = Counters { get_A :: !Int
                         , get_C :: !Int
                         , get_G :: !Int
                         , get_T :: !Int
                         }
dna strand = foldl go (Counters 0 0 0 0) strand
  where
    go (Counters a c g t) 'A' = Counters (a + 1) c g t 
    go (Counters a c g t) 'C' = Counters a (c +1) g t 
    go (Counters a c g t) 'G' = Counters a c (g + 1) t 
    go (Counters a c g t) 'T' = Counters a c g (t + 1)
    go _ c = error $ "bad strand '" ++ [c] ++ "'"
