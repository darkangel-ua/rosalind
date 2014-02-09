import System.Environment(getArgs)
import Data.List(isPrefixOf)

main :: IO ()
main = do
    (s1:s2:_) <- getArgs >>= read_lines
    mapM_ (\x -> (putStr $ show x) >> putStr " ") $ subs s1 s2
    putStrLn ""
  where
    read_lines (filename:_) = readFile filename >>= return . lines 
    read_lines _ = error "one argument required"

subs s t = go s 1 
  where 
    go [] _ = []
    go x@(_:s'') sp = if isPrefixOf t x 
                           then sp : go s'' (sp + 1)
                           else go s'' (sp + 1)


