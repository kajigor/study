module Chapter2.Ex_2_1 where

suffixes :: Eq a => [a] -> [[a]]
suffixes [] = [[]]
suffixes lst@(_:xs) = lst : suffixes xs