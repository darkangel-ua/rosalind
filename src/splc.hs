import System.Environment (getArgs) 
import Data.List (isPrefixOf, find)
import Data.Maybe(fromJust)
import qualified Data.HashMap.Strict as H

main :: IO ()
main = do
    c <- getArgs >>= readFile . head
    let ((_, strand):xs) = parse_fasta c
        introns = map snd xs
    putStrLn $ prot $ rna $ splice strand introns

splice :: String -> [String] -> String
splice strand introns = go strand 
  where
    il = zip introns (map length introns)
    go [] = []
    go s = maybe (head s : go (tail s)) 
                 (\(_, l) -> go (drop l s)) $ 
                 find (\(i, _) -> isPrefixOf i s) il

rna :: String -> String
rna = map $ \ch -> if ch == 'T' then 'U' else ch  
                    
rna_table :: H.HashMap String Char
rna_table = H.fromList [
            ("UUU", 'F'),      ("CUU", 'L'),      ("AUU", 'I'),      ("GUU", 'V'),
            ("UUC", 'F'),      ("CUC", 'L'),      ("AUC", 'I'),      ("GUC", 'V'),
            ("UUA", 'L'),      ("CUA", 'L'),      ("AUA", 'I'),      ("GUA", 'V'),
            ("UUG", 'L'),      ("CUG", 'L'),      ("AUG", 'M'),      ("GUG", 'V'),
            ("UCU", 'S'),      ("CCU", 'P'),      ("ACU", 'T'),      ("GCU", 'A'),
            ("UCC", 'S'),      ("CCC", 'P'),      ("ACC", 'T'),      ("GCC", 'A'),
            ("UCA", 'S'),      ("CCA", 'P'),      ("ACA", 'T'),      ("GCA", 'A'),
            ("UCG", 'S'),      ("CCG", 'P'),      ("ACG", 'T'),      ("GCG", 'A'),
            ("UAU", 'Y'),      ("CAU", 'H'),      ("AAU", 'N'),      ("GAU", 'D'),
            ("UAC", 'Y'),      ("CAC", 'H'),      ("AAC", 'N'),      ("GAC", 'D'),
            ("UAA", '_'),   ("CAA", 'Q'),      ("AAA", 'K'),      ("GAA", 'E'),
            ("UAG", '_'),   ("CAG", 'Q'),      ("AAG", 'K'),      ("GAG", 'E'),
            ("UGU", 'C'),      ("CGU", 'R'),      ("AGU", 'S'),      ("GGU", 'G'),
            ("UGC", 'C'),      ("CGC", 'R'),      ("AGC", 'S'),      ("GGC", 'G'),
            ("UGA", '_'),   ("CGA", 'R'),      ("AGA", 'R'),      ("GGA", 'G'),
            ("UGG", 'W'),      ("CGG", 'R'),      ("AGG", 'R'),      ("GGG", 'G')
            ]

prot :: String -> String
prot s = fst $ span (/= '_') $ map (\x -> fromJust $ H.lookup x rna_table) $ triplets s
  where
    triplets [] = []
    triplets x = let (x', xs) = splitAt 3 x in x' : triplets xs
      

-- simple parser that doesn't check any errors                                                                                                              
parse_fasta :: String -> [(String, String)]
parse_fasta content = go $ lines content                                                                                                                    
  where                                                                                                                                                     
    go [] = []                                                                                                                                              
    go (id_line:xs) = let (body, xs') = break (\(x:_) -> x == '>') xs                                                                                       
                      in make_one (tail id_line) body : go xs'                                                                                              
    make_one id_line body = (head $ words id_line, concat body)
