import System.Environment(getArgs)
import Data.List(foldl')

main :: IO ()
main = do
    (s1:s2:_) <- getArgs >>= read_lines
    print $ hamm s1 s2
  where
    read_lines (filename:_) = readFile filename >>= return . lines 
    read_lines _ = error "one argument required"

hamm :: String -> String -> Int
hamm s1 s2 = foldl' hf 0 $ zip s1 s2
  where
    hf acc (x, y) = if x == y then acc else acc + 1
