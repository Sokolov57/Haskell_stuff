doubleMe x = x + x 
doubleUs x y = x + y 

fib :: (Num a) => Int -> a
fib n 
      | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n-1) + fib (n-2)

newTake ::  Int -> Int -> [a] -> [a]  
newTake n m x
      | n <= 0 = []
      | m < 0 = []
      | m >= length x = []
      | m == 0 = take n x
      | otherwise = newTake n (m-1) (tail x)

 
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

ispart :: (Ord a, Num a) => a -> (Int -> a) -> Int -> Bool
ispart n f k
      | n == f k = True
      | n < f k = False
      | otherwise = ispart n f (k+1)

is :: (Ord a, Num a) => a -> (Int -> a) -> Bool
is n f = ispart n f 0


crazyFunction :: Int -> Int -> Int -> Int -> Int
crazyFunction a b c n 
      | n > b = n - c
      | n <= b = crazyFunction a b c (a + crazyFunction a b c (a + crazyFunction a b c (a + crazyFunction a b c (a + n) ) ) )


crazySum :: Int -> Int -> Int -> Int
crazySum a b c = (sum (map (`mod`(10^2)) (map (crazyFunction a b c) (map (`mod`(10^2)) (takeWhile (<= b) [0..])))))`mod`(10^2)


pathCount :: Int -> Int -> Int
pathCount 1 m = m + 1
pathCount n m = sum (map (pathCount (n-1) ) (takeWhile (<= m) [0..]))

quickpathCount :: Int -> Int -> Int
quickpathCount n m = c_a_b n (n+m)

take_primes ::  (Integral a) => a -> [a]
take_primes 2 = [2]
take_primes n 
      | (0`elem`(map (n`mod` ) (take_primes ((truncate (sqrt (fromIntegral (n))))+1)))) == True = take_primes (n-1)
      | otherwise = n:(take_primes(n-1))

make_coprime1 :: (Integral a) => a -> a -> [a] -> (a, a)  -- makes coquasiprime with some array of numbers, acting as basic
make_coprime1 a b (x:xs) 
      | xs == [] = let divide a b c = (if ((a`mod`c) == 0) && ((b`mod`c) == 0) then (a`div`c, b`div`c) else (a, b)) in ( if (fst (divide a b x)) == a then divide a b x  else make_coprime1 (fst (divide a b x)) (snd (divide a b x)) (x:xs) )
      | otherwise = let divide a b c = (if ((a`mod`c) == 0) && ((b`mod`c) == 0) then (a`div`c, b`div`c) else (a, b)) in make_coprime1 (fst (divide a b x)) (snd (divide a b x)) (if (fst (divide a b x)) == a then xs else x:xs) 

make_coprime :: (Integral a) => a -> a -> (a, a) -- obviously, makes 2 numbers coprime
make_coprime 1 b = (1,b)
make_coprime a 1 = (a,1)
make_coprime a b = make_coprime1 a b (take_primes (min a b))

f :: (Integral a) => a -> [a] -> (a, [a]) -- makes 1 number and a sequence coprime
f a [] = (a, [])
f a (x:xs) = (fst (f p_0 xs), p_1 : snd (f p_0 xs))
 where (p_0,p_1) = make_coprime a x

g :: (Integral a) => [a] -> [a] -> ([a], [a]) -- makes 2 sequences coprime 
g xs [] = (xs,[])
g [] ys = ([],ys)
g (x:[]) ys = (fst (f x ys):[], snd (f x ys) )
g (x:xs) (y:ys) = (f_0:g_0, g_1)
 where (f_0, f_1) = f x (y:ys)
       (g_0,g_1) = g xs f_1

c_a_b :: (Integral a) => a -> a -> a  --combinations, it is actually C^a_b
c_a_b 0 0 = 0
c_a_b 0 b = 1
c_a_b a b = product (fst (g [((max a (b-a))+1)..b] [2..(min a (b-a))]))
