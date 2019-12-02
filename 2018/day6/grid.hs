import Data.Maybe
import Data.List
import qualified Data.Map as M
import Debug.Trace

points :: IO [(Int, Int)]
points = do
  input <- readFile "input"
  return $ read . (\s -> '(':s ++ ")") <$> lines input

main = do
  ps <- points
  let minX = minimum $ map fst ps
  let minY = minimum $ map snd ps
  let maxX = maximum $ map fst ps
  let maxY = maximum $ map snd ps
  let initialGrid =  M.fromList [((x,y), Un) | x <- [minX .. maxX], y <- [minY .. maxY]]
  let grid = M.mapWithKey (processPoint ps) initialGrid :: M.Map (Int, Int) Status
  -- putStr $ showGrid ps grid
  let edges = M.filterWithKey (\k _ -> fst k == minX || fst k == maxX || snd k == minY || snd k == maxY ) grid
  let infs = M.map (\(Closest p) -> p) $ M.filter (/= Tied) edges :: M.Map (Int, Int) (Int, Int)
  -- Keep only those points which do not have an edge element as their closest.

  let increment m x = M.insert x (1 + (maybe 0 id (M.lookup x m :: Maybe Int)) :: Int) m
  let put m p = case p of
              Closest p' -> increment m p'
              Tied -> m
  let count = foldl' put M.empty (M.elems grid)
  let finiteCount = foldr M.delete count (M.elems infs)
  let maxArea = M.foldl max 0 finiteCount
  print maxArea

  -- part 2
  let grid2 = M.mapWithKey (processPoint2 ps) initialGrid :: M.Map (Int, Int) Safe
  let safePoints = M.filter (==Yes) grid2
  print $ M.size safePoints


showGrid :: [(Int, Int)] -> M.Map (Int, Int) Status -> String
showGrid ps m = unlines $ map unwords $ map (map (snd)) $ groupBy (\(p1,_) (p2,_) -> fst p1 == fst p2) $ sort $ M.toList $ M.map shower m
  where
    shower Tied = " . "
    shower (Closest p) = show $ (100 +) $ fromJust $ elemIndex p ps

processPoint :: [(Int, Int)] -> (Int, Int) -> Status -> Status
processPoint ps (x, y) _ = let dists = sort $ map (\(px, py) -> ( abs (px - x) + abs (py - y), (px, py))) ps in
  case dists of
    ((d1, p):(d2, _):_) | d1 == d2  -> Tied
                        | otherwise -> Closest p

processPoint2 :: [(Int, Int)] -> (Int, Int) -> Status -> Safe
processPoint2 ps (x, y) _ = let totalDist = sum $ map (\(px, py) -> ( abs (px - x) + abs (py - y))) ps in
  if totalDist < 10000 then Yes else No

data Status = Un | Closest (Int , Int) | Tied deriving (Show, Eq)
data Safe = Yes | No deriving Eq
