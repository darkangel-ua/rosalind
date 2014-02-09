import Control.Monad (forM_, when)
import Network.HTTP
import System.Environment(getArgs)
import Text.Regex.Posix
import Data.Array (elems)
import Control.Concurrent.Async(mapConcurrently)

(|>) :: a -> (a -> b) -> b
(|>) x y = y x
 
main :: IO ()
main = do
    ids <- getArgs >>= readFile . head >>= return . lines
    motifs <- mapConcurrently findMotifs ids
    forM_ (zip ids motifs) $ \(prot_id, offsets) -> do
        when (not $ null offsets) $ do
            putStrLn prot_id
            forM_ offsets $ \x -> putStr $ show x ++ " " 
            putStrLn ""
  where 
    loadFile prot_id = do
        let prim_id = takeWhile ( /= '_') prot_id
        c <- simpleHTTP (getRequest $ "http://www.uniprot.org/uniprot/" ++ prim_id ++ ".fasta") >>= getResponseBody
        return $ lines c |> tail |> concat
    findMotifs prot_id = do
        protein <- loadFile prot_id
        let pattern = makeRegex "N[^P](S|T)[^P]" :: Regex
        return $ matchAll pattern protein |> map (\x -> elems x |> head |> fst |> succ) 
