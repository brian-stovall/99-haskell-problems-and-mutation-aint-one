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

--7
data NestedList a = Elem a | List [NestedList a] deriving Show

flatten' :: NestedList a -> [a]
flatten' (List []) = []
flatten' (Elem a)  = [a]
flatten' (List (x:xs)) = flatten' x ++ flatten' (List xs)

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

--11
data Encoding a = Single a | Multiple (Int, a) deriving Show

encode' :: (Eq a) => [a] -> [Encoding a]
encode' [] = []
encode' as = map helper . encode $ as
  where
    helper (1,x) = Single x
    helper (n,x) = Multiple (n,x)

--12
decodeOne :: Encoding a -> [a]
decodeOne (Single a) = [a]
decodeOne (Multiple (n, a)) = repeat' n a

decode :: [Encoding a] -> [a]
decode xs = concatMap decodeOne xs

--13 

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

--18
slice :: Int -> Int -> [a] -> [a]
slice m n as = take (n - m + 1) . drop (m - 1) $ as

--19
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 ls = ls
rotate n ls@(a:as)
  | (abs n) > size = rotate (rem (abs n) size) ls
  | n < 0         = rotate (size + n) ls
  | otherwise     = rotate (n - 1) (as ++ [a])
    where size = length ls

--20
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Call to removeAt with empty list"
removeAt n ls = (del, (init front) ++ back)
  where front = take n ls
        back  = drop n ls
        del   = last front

--21
insertAt :: a -> [a] -> Int -> [a]
insertAt a as n = take (n - 1) as ++ [a] ++ drop (n - 1) as

--22
range :: Int -> Int -> [Int]
range x y = [x..y]

--23 - 30 TODO

--31
isPrime :: Int -> Bool
isPrime n
  | n == 1       = False
  | n == 2       = True
  | n == 3       = True
  | rem n 2 == 0 = False
  | otherwise    = primeHelper n 3

primeHelper :: Int -> Int -> Bool
primeHelper n cur
  | rem n cur == 0  = False
  | (cur * cur) > n = True
  | otherwise       = primeHelper n (cur + 2)

--32
gcd' :: Int -> Int -> Int
gcd' x y
  | rem big small == 0 = small
  | otherwise          = gcd' (big - small) small
    where
      big = max x y
      small = min x y
