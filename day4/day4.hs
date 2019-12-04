-- So simple in Haskell it would be a shame not to do it

main = do print $ length $ filter increasing $ filter repeatingDigits a
          print $ length $ filter increasing $ filter repeatingDigits2 a

a = [264793..803935] -- My input

increasing n = decreasingBackwards 9 n
decreasingBackwards last n
  | n < 10    = last >= n
  | otherwise = let n' = n `mod` 10 in
                    last >= n' && decreasingBackwards n' (n `div` 10) 

repeatingDigits n
  | n < 10    = False
  | otherwise = (n `mod` 10 == (n `div` 10) `mod` 10 || repeatingDigits (n `div` 10))

repeatingDigits2 n
  | n < 10    = False
  | n `mod` 10 == (n `div` 10) `mod` 10 && n `mod` 10 == (n `div` 100) `mod` 10 = repeatingDigits2 (strip (n `mod` 10) n)
  | otherwise = (n `mod` 10 == (n `div` 10) `mod` 10 || repeatingDigits2 (n `div` 10))
  where
      strip _ 0 = 0
      strip i n = if n `mod` 10 == i then strip i (n `div` 10) else n
