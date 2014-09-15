import qualified Data.List as L
import qualified System.Random as R
import Data.Function
import Data.Maybe

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
data Run a = Mulappendle Int a | Single a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Run a]
encodeModified = (map encoder) . pack
  where encoder (x:xs)
          | null xs = Single x
          | otherwise = Mulappendle (1+length xs) x

-- 12
decodeModified :: [Run a] -> [a]
decodeModified [Single x] = [x]
decodeModified (Single x : zs) = x : decodeModified zs
decodeModified (Mulappendle c x : zs)
  | c > 2 = x : decodeModified (Mulappendle (c-1) x : zs)
  | otherwise = x : decodeModified (Single x : zs)

-- 13
encodeDirect :: (Eq a) => [a] -> [Run a]
encodeDirect = foldr encoder []
  where encoder x [] = [Single x]
        encoder x zs'@(Single y : zs)
          | x == y = Mulappendle 2 y : zs
          | otherwise = Single x : zs'
        encoder x zs'@(Mulappendle c y : zs)
          | x == y = Mulappendle (c+1) y : zs
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
  where rnd xs k gen
          | k == 1 = [xs !! randKey]
          | otherwise = (xs !! randKey) : rnd xs (k-1) newGen
          where (randKey, newGen) = R.randomR (0, length xs - 1) gen

-- 24
diff_select :: (Eq a) => [a] -> Int -> [a]
diff_select xs k = rnd xs k (R.mkStdGen 0)
  where rnd xs k gen
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
  where shorters = filter (\y -> length y < length x) xs
        longers = filter (\y -> length y >= length x) xs

lfsort :: [[a]] -> [[a]]
lfsort xs =
  let sortByLength = lsort xs
      groupByLength = L.groupBy ((==) `on` length) sortByLength
      groupByLengthFreq = lsort groupByLength
  in concat groupByLengthFreq

-- 31
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all ((/=0) . rem n) $ [2 .. floor $ sqrt $ fromIntegral n]

-- 32
myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b $ mod a b

-- 33
coprime :: Int -> Int -> Bool
coprime a b = 1 == myGcd a b

-- 34
totient :: Int -> Int
totient n = length $ filter (coprime n) $ range 1 n

-- 35
primeFactors :: Int -> [Int]
primeFactors n = primeEnum primes n
  where primes = filter isPrime $ range 2 n
        primeEnum [] k = []
        primeEnum _ 1 = []
        primeEnum ps'@(p:ps) k
          | k `rem` p == 0 = p : primeEnum ps' (k `div` p)
          | otherwise = primeEnum ps k

-- 36
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = map (\xs@(x:_) -> (x, length xs)) $ L.group $ primeFactors n

-- 37
phi :: Int -> Int
phi n = foldl (*) 1 $ map (\(p, m) -> (p-1) * p^(m-1)) $ prime_factors_mult n

-- 39
primesR :: Int -> Int -> [Int]
primesR a b = filter (isPrime) $ range a b

-- 40
goldbach :: Int -> (Int, Int)
goldbach n = goldbach_test (primesR 2 n)  n
  where goldbach_test (x:xs) n
          | n <= x = goldbach_test xs n
          | isPrime (n-x) = (x, n-x)
          | otherwise = goldbach_test xs n

-- 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach $ filter even $ range (max 4 a) b

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b m = map (\(Just x) -> x) $ has_goldbach
    where validRange = filter even $ range (max 4 a) b
          goldbacher = goldbach_test $ primesR m b
          has_goldbach = filter isJust $ map goldbacher validRange
          goldbach_test (x:xs) n
            | null xs = Nothing
            | n <= x || (n-x) <= m = goldbach_test xs n
            | isPrime (n-x) = Just (x, n-x)
            | otherwise = goldbach_test xs n

goldbachList'' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList'' a b m = filter (\(x,y) -> m <= min x y) $ goldbachList a b

-- 46
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' a b = not $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not $ or' a b

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
equ' a b = not $ xor' a b

table :: (Bool -> Bool -> Bool) -> IO ()
table pred = mapM_ putStrLn [concat [show a, "\t", show b, "\t", show $ pred a b] | a <- tf, b <- tf]
  where tf = [True, False]

-- 47
-- did not understand this problem, taken from solution to proceed
infixl 4 `or'`
infixl 6 `and'`

bools :: Int -> [[Bool]]
bools k
    | k == 1 = [[True], [False]]
    | otherwise = concat [map ([True]++) next, map ([False]++) next]
  where next = bools (k-1)

-- 48
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n pred = mapM_ putStrLn tableLines
  where tableLines = map (concat . cells) $ bools n
        cells xs = [L.intercalate "\t" $ map show xs, "\t", (show . pred) xs]

-- 49
gray :: Int -> [String]
gray n
  | n == 1 = ["0", "1"]
  | otherwise = concat [map (++"0") $ reverse next, map (++"1") next]
  where next = gray (n-1)

-- 50
data Huff a = Leaf a | Node (Huff a) (Huff a) deriving (Show)
sortSnd = L.sortBy (compare `on` snd)

readHuff :: String -> Huff a -> [(a, String)]
readHuff p (Leaf l) = [(l, p)]
readHuff p (Node left right) = (readHuff (p++"0") left) ++ (readHuff (p++"1") right)

-- less efficient version cause I don't know how to use priority queue in Haskell
genHuffman :: [(Huff a, Int)] -> [(a, String)]
huffman :: [(Char, Int)] -> [(Char, String)]

genHuffman [(t,w)] = readHuff "" t
genHuffman ((t1,w1):(t2,w2):qs) = genHuffman $ sortSnd $ (Node t1 t2,w1+w2):qs

huffman xs = genHuffman (map (\(c,w) -> (Leaf c,w)) $ sortSnd xs)

-- more efficient but messier
genHuffman' :: [(Huff a, Int)] -> [(Huff a, Int)] -> [(a, String)]
huffman' :: [(Char, Int)] -> [(Char, String)]

genHuffman' [] [(t,_)] = readHuff "" t
genHuffman' [(t,_)] [] = readHuff "" t
genHuffman' [(tl,_)] [(tr,_)] = readHuff "" (Node tl tr)
genHuffman' ((t1,w1):(t2,w2):ql) [] = genHuffman' ql [append]
  where append = (Node t1 t2, w1+w2)
genHuffman' [] ((t1,w1):(t2,w2):qr) = genHuffman' [] $ qr ++ [append]
  where append = (Node t1 t2, w1+w2)
genHuffman' l@(l1:l2:ql) r@(r1:r2:qr)
  | wl2 < wr1 = genHuffman' ql (r ++ [(Node tl1 tl2, wl1+wl2)])
  | wr2 < wl1 = genHuffman' l (qr ++ [(Node tr1 tr2, wr1+wr2)])
  | otherwise = genHuffman' (l2:ql) ((r2:qr) ++ [(Node tl1 tr1, wl1+wr1)])
  where (tl1,wl1) = l1
        (tl2,wl2) = l2
        (tr1,wr1) = r1
        (tr2,wr2) = r2

huffman' xs = genHuffman' (map (\(c,w) -> (Leaf c,w)) $ sortSnd xs) []

-- 55
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving Show
leaf x = Branch x Empty Empty

isBalancedTree :: Tree a -> Bool
isBalancedTree Empty = True
isBalancedTree (Branch _ left right) = (abs $ (children left) - (children right)) <= 1
  where children Empty = 1
        children (Branch _ left right) = children left + children right

t1 = Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))
t2 = Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty)
t3 = Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)
t4 = Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
t5 = Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) Empty

cbalTree :: Int -> [Tree Char]
cbalTree n
  | n < 1 = [Empty]
  | n == 1 = [leaf 'x']
  | fl == cl = [Branch 'x' l r | l <- t1, r <- t2]
  | otherwise = map (\(l, r) -> Branch 'x' l r) subTrees
  where fl = (n-1) `div` 2
        cl = n - fl - 1
        t1 = cbalTree fl
        t2 = cbalTree cl
        subTrees = [p | l <- t1, r <- t2, p <- [(l, r), (r, l)]]

-- 56
mirror :: Tree a -> Tree b -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 r2 && mirror l2 r1
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric t = mirror t t

-- 57
add :: Tree Int -> Int -> Tree Int
add Empty x = Branch x Empty Empty
add t@(Branch v l r) x
  | x < v = Branch v (add l x) r
  | x > v = Branch v l (add r x)
  | otherwise = t

construct :: [Int] -> Tree Int
construct = foldl add Empty

-- 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees n
  | even n = []
  | otherwise = filter symmetric $ cbalTree n

-- 59
isHgtBalanced :: Tree a -> Bool
isHgtBalanced Empty = True
isHgtBalanced (Branch _ left right) = (abs $ maxHgt left - maxHgt right) <= 1
  where maxHgt Empty = 0
        maxHgt (Branch _ left right) = max (1+maxHgt left) (1+maxHgt right)

hbalTree :: a -> Int -> [Tree a]
hbalTree x 0 = [Empty]
hbalTree x 1 = [leaf x]
hbalTree x h = map (\(l, r) -> Branch x l r) subTrees
  where s1 = hbalTree x $ h-1
        s1' = hbalTree x $ h-1
        s2 = hbalTree x $ h-2
        subTrees = [p | a <- s1, b <- s2, p <- [(a, b), (b, a)]] ++ [(a, b) | a <- s1', b <- s1]

-- 60
-- upper bound from http://www.cs.umd.edu/~meesh/351/mount/lectures/extra-AVL.pdf
nodeCount :: Tree a -> Int
nodeCount Empty = 0
nodeCount (Branch _ l r) = 1 + nodeCount l + nodeCount r

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = filter (\t -> nodeCount t == n) $ concat $ map (hbalTree x) $ [minHgt..maxHgt]
  where minHgt = ceiling $ logBase 2 $ fromIntegral n+1
        maxHgt = length (takeWhile (<n) fibs) -3
          where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- 61
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- 61a
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

-- 62
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = x:internals l ++ internals r

-- 62b
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch _ l r) h = atLevel l (h-1) ++ atLevel r (h-1)

-- 63
completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree 1 = leaf 'x'
completeBinaryTree n = tree 1
  where tree m
          | n < m = Empty
          | otherwise = Branch 'x' (tree $ 2*m) (tree $ 2*m+1)

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree t = (fst $ completeHgt t) >= 0
  where completeHgt Empty = (0, True)
        completeHgt (Branch _ l' r')
          | rFull && lHgt == rHgt + 1 = (1 + lHgt, False)
          | lFull && lHgt == rHgt = (1 + lHgt, rFull)
          | otherwise = (-1, False)
          where (lHgt, lFull) = completeHgt l'
                (rHgt, rFull) = completeHgt r'
