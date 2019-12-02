import Data.Char

main =
    do
        input <- readFile "input.ply"
        let res = process input
        putStrLn res
        return $ length res
        putStrLn "Filters"
        let filts = processFiltered res
        print filts
        print $ minimum $ map length filts


process :: String -> String
process inp = process' (filter (isAlpha) inp) []

process' :: String -> String -> String
process' (x:xs) [] = process' xs [x]
process' (x:xs) (y:ys) | isLower x /= isLower y && toLower x == toLower y = process' xs ys
  | otherwise = process' xs (x:y:ys) 
process' [] ys = ys

processFiltered inp = map process [(filter (\x -> toLower x /= c) inp) | c <- ['a'..'z']]
