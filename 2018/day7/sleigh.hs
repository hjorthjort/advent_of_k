import qualified Data.Map as M
import Data.List
import Debug.Trace
import Data.Char

parse :: String -> [(Char, Char)]
parse s =
    let ls = lines s in
        map mkPair ls
            where
                mkPair line = (line !! 36, line !! 5)

main = do
    input <- readFile "input"
    let deps = mkDepsMap $ parse input
    trace (show deps) $ print $ loop2 deps

loop :: M.Map Char [Char] -> [Char]
loop deps | M.size deps == 0 = []
          | otherwise =
    let ready = head $ M.keys $ M.filter null deps
        deps' =  M.map (delete ready) $ M.delete ready deps
        in 
    ready:loop deps'


loop2 :: M.Map Char [Char] -> Int
loop2 = loop2' 5 [] 0
-- Either delegate work or take a time step.
loop2' n ws sum deps | M.size deps == 0 && null ws = sum - 1
                 | otherwise =
     let completed = filter (\(r, t) -> t == 0) ws
         deps' =  foldl' (\m (ready, t) -> M.map (delete ready) m) deps completed
         n' = n + length completed
         ws' = filter (\(r, t) -> t /= 0) ws
         readies = M.keys $ M.filter null deps'  in
         case (n', readies) of
           (0, _) -> loop2' 0 (tic ws') (1 + sum) deps'
           (_, []) -> loop2' n' (tic ws') (1 + sum) deps'
           (_, (ready:rs)) -> loop2' (n'-1) ((ready, ord ready - 4):ws') sum (M.delete ready deps')
   where tic = map (\(r, time) -> (r, time - 1))

mkDepsMap :: [(Char, Char)] -> M.Map Char [Char]
mkDepsMap pairs =
    let sorted = sort pairs
        groups = groupBy (\(f, _) (f', _) -> f == f') sorted :: [[(Char, Char)]]
        list = map (\g -> ((fst.head) g, map snd g)) groups :: [(Char, [Char])]
        depsMap = M.fromList list in
    foldl' (\m (_,c) -> if M.member c m then m else M.insert c [] m) depsMap sorted

    


