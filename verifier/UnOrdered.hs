module UnOrdered where

takeRepeat a= take 1000000 $ repeat a

-- This is a function that replaces the nth value of a list
replaceNth n newVal (x:xs)
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs