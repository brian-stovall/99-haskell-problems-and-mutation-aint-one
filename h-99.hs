myLast :: (Eq a) => [a] -> a
myLast [] = error "No last element for an empty list!"
myLast (x:xs)
  | xs == []  = x
  | otherwise = myLast(xs)

myButLast :: [a] -> a
myButLast [] = error "No next-to-last element in empty list!"
myButLast [x] = error "No next-to-last element in list of one element!"
myButLast [x:xs]
  | length xs == 1 = x
  | otherwise = myButLast xs
