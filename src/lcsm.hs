{-# LANGUAGE BangPatterns #-}

import System.Environment     (getArgs)
import Data.STRef
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as H
import Control.Applicative
import Control.Monad (foldM)
import qualified Data.Vector.Mutable as V

main :: IO ()
main = do
    c <- getArgs >>= readFile . head
    let x = map snd $ parse_fasta c
    print x
    
--    c <- getArgs >>= readFile . head
--    let (x:xs) = map snd $ parse_fasta c
--        cs = map (\v -> rlcstr (B.pack x) v) $ xs
--        reduced = foldl1 (\a y -> filter (\z -> all (isInfixOf z) y) a) cs 
--    print cs
--    print $ sort $ map reverse reduced

-- reversed longest substrings
--rlcstr :: B.ByteString -> String -> [String]
--rlcstr l r = let max_length = maximum $ map (UV.maximum) $ V.toList m
--             in map head $ group $ sort $ [ mkSubstring x y | y <- [0..V.length m - 1], x <- [0..B.length l], at x y > 1]  
--  where
--    mkSubstring x y = if (at x y /= 0) then B.index l (x - 1) : mkSubstring (x - 1) (y - 1) else []  
--    at x' y' = (UV.!) ((V.!) m y') x' 
--    m = let first = UV.replicate (B.length l + 1) (0 :: Int)
--        in V.fromList $ go first r 
--      where
--        go prev [] = [prev] 
--        go prev (c:xs) = prev : go curr xs
--          where 
--            curr = UV.create $ do
--                   v <- MV.replicate (B.length l + 1) 0 
--                   forM_ [1..B.length l] $ \i ->
--                       if c == B.index l (i - 1) 
--                           then MV.write v i $ (UV.!) prev (i - 1) + 1 
--                           else MV.write v i 0 
--                   return v 
    
-- simple parser that doesn't check any errors                                                                                                              
parse_fasta :: String -> [(String, String)]
parse_fasta content = go $ lines content                                                                                                                    
  where                                                                                                                                                     
    go [] = []                                                                                                                                              
    go (id_line:xs) = let (body, xs') = break (\(x:_) -> x == '>') xs                                                                                       
                      in make_one (tail id_line) body : go xs'                                                                                              
    make_one id_line body = (head $ words id_line, concat body)
    
type Nexts s = H.HashTable s Char Int
data Suffix = Suffix !Int !Int
type RSuffix s = STRef s Suffix  
data Node s = Node { suffix :: RSuffix s, nexts :: Nexts s, link :: !Int } 
data GVector s = GVector { vector :: V.STVector s (Node s), capacity :: !Int } 
data State s = State { nodes :: GVector s, root_node :: !Int, pos :: !Int, remainder :: !Int, active_node :: !Int, active_length :: !Int }   

gvNew c = do 
    v <- V.new c
    return $ GVector v c
 
--makeGenericSuffixTree :: String -> ST s (State s)
--makeGenericSuffixTree str = do
--    root <- mkRoot
--    nodes' <- gvNew 1024 
--    foldM go (State nodes' 0 0 1 0 0) str
--  where 
--    mkRoot = mkNode (-1)
--    mkNode p = Node <$> newSTRef (Suffix p (-1)) <*> H.new <*> (-1) 
--    go s c = do
--        n <- mkNode (pos s) 
--        H.insert (nexts . root_node $ s) c n 
--        return s{pos = pos s + 1}