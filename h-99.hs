-- 1
myLast :: (Eq a) => [a] -> a
myLast [] = error "No last element for an empty list!"
myLast (x:xs)
  | xs == []  = x
  | otherwise = myLast(xs)

--2
myButLast :: [a] -> a
myButLast [] = error "No next-to-last element in empty list!"
myButLast [x] = error "No next-to-last element in list of one element!"
myButLast (x:xs)
  | length xs == 1 = x
  | otherwise = myButLast xs

--3
elementAt' :: Int -> [a] -> a
elementAt' _ [] = error "List shorter than requested element!"
elementAt' n (x:xs)
  | n < 1 = error "Element requested must be >= 1!"
  | n == 1    = x
  | otherwise = elementAt' (n-1) xs

--4
myLength :: [a] -> Int
myLength as = foldl (\acc _-> acc + 1) 0 as 

--5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

--6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome as
  | as == myReverse as = True
  | otherwise          = False

--7 TODO

--8
compress :: (Eq a) => [a] -> [a]
compress (a:[]) = [a]
compress (a:as) = if (a == head as) then compress as else a : (compress as)

--9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack as@(a:_) = takeWhile (== a) as : (pack $ dropWhile (== a) as)

--10
encode :: (Eq a) => [a] -> [(Int, a)]
encode as = map (\as -> (length as, head as)) $ pack as

--11-13 TODO

--14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

--15
repli :: Int -> [a] -> [a]
repli 0 _  = []
repli _ [] = []
repli n (a:as) 
  | n == 0    = []
  | otherwise = (repeat' n a) ++ (repli n as)

repeat' :: Int -> a -> [a]
repeat' 0 _ = []
repeat' n x = x : repeat' (n-1) x

--16
dropEvery :: Int -> [a] -> [a]
dropEvery 0 as = as
dropEvery _ [] = []
dropEvery n as = take (n-1) as ++ dropEvery n (drop (n) as)

--17
split :: Int -> [a] -> ([a], [a])
split n as = (take n as, drop n as)

--without take/drop
split' :: Int -> [a] -> ([a], [a])
split' n [] = ([],[])
split' n ls@(a:as)
  | n == 0    = ([], ls)
  | otherwise = (a : fst (sep), snd (sep))
    where sep = split' (n-1) as

