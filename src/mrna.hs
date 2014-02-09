import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad (foldM)
import Data.Char(ord)
import Data.List (foldl')

main :: IO ()
main = do
  p <- getLine
  print $ mrna p
  
mrna :: String -> Int
mrna = foldl' go 3
  where
    go a c = a * V.unsafeIndex freq_table (ord c) `mod` 1000000 

rna_table :: [([Char], Char)]
rna_table = [
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
            
freq_table :: V.Vector Int
freq_table = V.create $ do
    v <- MV.replicate 128 0
    foldM go v rna_table
  where
    go v (_, c) = do
        x <- MV.read v $ ord c 
        MV.unsafeWrite v (ord c) (succ x)
        return v