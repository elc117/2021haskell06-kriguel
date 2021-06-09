-- Prática 06 de Haskell
-- Nome: Leonardo Cargnin Krugel


-- 1)
ends :: [Int] -> [Int]
ends list =  head list : last list : []


-- 2)
deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = 2*x : deduzame xs


-- 3)
deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2
  then x : deduzame2 xs
  else deduzame2 xs


-- 4)
geraTabela :: Int -> [(Int,Int)]
geraTabela n
    | n < 1 = []
    | otherwise = (n,n*n) : geraTabela (n-1)


-- 5)
contido :: Char -> String -> Bool
contido c (x:xs) 
    | x == c = True
    | xs == [] = False    
    | otherwise = contido c xs


-- 6)
translate :: [(Float,Float)] -> [(Float,Float)]
translate [] = []
translate ((x,y):xs) = (x+2,y+2) : translate xs


-- 7)
countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs)
    | length x > 5 = 1 + countLongs xs
    | otherwise = countLongs xs


-- 8)
onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs)
    | length x > 5 = x : onlyLongs xs
    | otherwise = onlyLongs xs
