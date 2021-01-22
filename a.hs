import Control.Monad

sum_elements::(Integral a)=> [a]->[a]->[a]
sum_elements (x1:x2:xs) res = sum_elements ([x2]++xs) (res++[(x1+x2)])
sum_elements _ res = res 
 

pascal_triangle' :: Integral a => a -> [a] -> [[a]] -> [[a]]
pascal_triangle' 0 li_1 res = res
pascal_triangle' i li_1 res = pascal_triangle' (i-1) li (res++[li])
	where 
		li = [1] ++ (sum_elements li_1 []) ++ [1]

pascal_triangle :: Integral a => a -> [[a]]
pascal_triangle i = pascal_triangle' (i-1) [] [[1]]


main = do
  input <- getLine
  mapM_ print $ join $ pascal_triangle . listToSingle. convertToInt . words $ input
 where
  listToSingle(x:_) = x
  convertToInt = map (read :: String -> Int)
