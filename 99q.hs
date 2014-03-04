import qualified Data.List as L
import qualified System.Random as R
import Data.Function

-- 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) k = elementAt xs $ k-1

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (isPalindrome $ init xs) && x == myLast xs

-- 7
data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten :: (Show a) => NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:ys'@(y:ys))
    | x == y    = compress ys'
    | otherwise = x:compress ys'

-- 9
pack :: (Eq a) => [a] -> [[a]]
pack = foldr packer []
    where packer x [] = [[x]]
          packer x (ys'@(y:_):zs)
            | y == x = (x:ys'):zs 
            | otherwise = [x] : (ys':zs)

-- 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode = (map (\xs@(x:_) -> (length xs, x))) . pack

-- 11
data Run a = Multiple Int a | Single a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Run a]
encodeModified = (map encoder) . pack
    where encoder (x:xs)
            | null xs = Single x
            | otherwise = Multiple (1+length xs) x

-- 12
decodeModified :: [Run a] -> [a]
decodeModified [Single x] = [x]
decodeModified (Single x : zs) = x : decodeModified zs
decodeModified (Multiple c x : zs)
    | c > 2 = x : decodeModified (Multiple (c-1) x : zs)
    | otherwise = x : decodeModified (Single x : zs)

-- 13
encodeDirect :: (Eq a) => [a] -> [Run a]
encodeDirect = foldr encoder []
    where encoder x [] = [Single x]
          encoder x zs'@(Single y : zs)
            | x == y = Multiple 2 y : zs
            | otherwise = Single x : zs'
          encoder x zs'@(Multiple c y : zs)
            | x == y = Multiple (c+1) y : zs
            | otherwise = Single x : zs'

-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x : dupli xs

-- 15
repli :: [a] -> Int -> [a]
repli [] k = []
repli (x:xs) k = copy x k ++ (repli xs k)
    where copy x 1 = [x]
          copy x k = x : copy x (k-1)

repli' xs k = concat $ map (replicate k) xs

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs k = dropHelper xs k k
    where dropHelper [] _ _ = []
          dropHelper (y:ys) k n
            | k > 1 = y : dropHelper ys (k-1) n
            | otherwise = dropHelper ys n n

-- 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs'@(x:xs) k 
    | k == 0 = ([], xs')
    | k < 0 = split xs' (length xs' + k + 1)
    | otherwise = (x : fst (split xs (k-1)), snd (split xs (k-1)))

-- 18
slice :: [a] -> Int -> Int -> [a]
slice xs a b =
    let (ls, _) = split xs b
        (_, c) = split ls (a-1)
    in  c

-- 19
rotate :: [a] -> Int -> [a]
rotate xs k = 
    let (l, r) = split xs k
    in  r ++ l

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt k xs =
    let (l, c:_) = split xs (k-1)
        (_, r) = split xs k
    in  (c, l ++ r)

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs k =
    let (l, r) = split xs (k-1)
    in  l ++ [x] ++ r

-- 22
range :: (Ord a, Enum a) => a -> a -> [a]
range a b
    | a == b = [a]
    | a < b = a : range (succ a) b
    | a > b = a : range (pred a) b

-- 23
rnd_select :: [a] -> Int -> [a]
rnd_select xs k = rnd xs k (R.mkStdGen 0)
    where 
        rnd xs k gen
            | k == 1 = [xs !! randKey]
            | otherwise = (xs !! randKey) : rnd xs (k-1) newGen 
            where
                (randKey, newGen) = R.randomR (0, length xs - 1) gen

-- 24
diff_select :: (Eq a) => [a] -> Int -> [a]
diff_select xs k = rnd xs k (R.mkStdGen 0)
    where 
        rnd xs k gen
            | k == 1 = [xs !! randKey]
            | otherwise = (xs !! randKey) : rnd (L.delete (xs !! randKey)  xs) (k-1) newGen 
            where (randKey, newGen) = R.randomR (0, length xs - 1) gen
        

-- 25
rnd_permu :: (Eq a) => [a] -> [a]
rnd_permu xs = diff_select xs (length xs)

-- 26
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _ = [[]]
combinations 1 (x:xs) = [x] : combinations 1 xs
combinations k xs'@(x:xs) =
    map ([x]++) (combinations (k-1) xs) ++ combinations k xs

-- 28
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = lsort shorters ++ [x] ++ lsort longers
    where 
        shorters = filter (\y -> length y < length x) xs
        longers = filter (\y -> length y >= length x) xs

lfsort :: [[a]] -> [[a]]
lfsort xs =
    let sortByLength = lsort xs
        groupByLength = L.groupBy ((==) `on` length) sortByLength
        groupByLengthFreq = lsort groupByLength
    in concat groupByLengthFreq

-- 31
isPrime :: Int -> Bool
isPrime n = and $ tail $ map ((/=0) . rem n) $ range 1 $ round $ sqrt $ fromIntegral n

-- 32
myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b $ mod a b

-- 33
coprime :: Int -> Int -> Bool
coprime a b = 1 == myGcd a b

totient :: Int -> Int
totient n = length $ filter (coprime n) $ range 1 n