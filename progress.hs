module Progress where
import Data.List 
import System.Random

-- Returns the max dotproduct between a list and a list of lists
filter_vector :: [Int] -> [[Int]] -> Int
filter_vector v1 vectorlist  =  maximum [ dotproduct x v1 | x <- vectorlist]

-- Returns the max dotprod between lists, using filter_vector
filter_dotprod :: [[Int]] -> Int
filter_dotprod [x] = 0
filter_dotprod (x:xs) = maximum $ filter_vector x xs:[filter_dotprod xs]

-- Returns the same function without duplicates
unique :: Eq a => [a] -> [a]
unique []       = []
unique (x : xs) = x : unique (filter (x /=) xs)

-- This is some kind of dotproduct
dotproduct :: [Int] -> [Int] -> Int
dotproduct list1 list2 = length $ list1 `intersect` list2

delete_lists :: (Eq a) => [a] -> [a] -> [a]
delete_lists [] list = list
delete_lists (x:xs) big_list = delete_lists xs $ delete x big_list 

guilty_ones :: [[Int]] -> Int -> [[Int]]
guilty_ones list limit = unique [ x | x <- list, y <- list, limit == dotproduct x y]

exclusiveDice :: Int -> [Int] -> IO Int
exclusiveDice limit tabu = do
    num <- newStdGen
    let (x,_) = randomR(1,limit) num
        list  = delete_lists tabu [1..limit]
    return $ list !! (x `mod` length(list))

rand :: Int -> Int -> [Int]->  IO [Int]
rand _ 0 _ = return []
rand limit len tabu= do  
    x <- (exclusiveDice limit tabu)
    xs <- (rand limit (len-1) (x:tabu))
    return $ x :xs

randlists :: Int -> Int -> Int -> IO [[Int]]
randlists 0 _ _ = return []
randlists rows columns r = do
    list <- (rand columns r [])
    rest <- (randlists (rows-1) columns r)
    return $ list:rest

localID :: Int -> Int -> Int -> IO [[Int]]
localID rows columns r = do 
    first_solution <- (randlists rows columns r)
    return $ [filter_dotprod first_solution]:first_solution

neighbors :: [[Int]] -> [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
neighbors [] list tabu _ curr_sol = []
neighbors (element:rest) list tabu limit curr_sol =
    let (x:xs) = element
        list2 = delete element list 
        options = delete_lists (x:xs) [1..limit]
        ans = searchBest element list2 options tabu [] curr_sol
    in (ans: neighbors rest (ans:list2) (ans:tabu) limit curr_sol)



--searchRandom :: [Int] -> [Int] -> IO [Int]
--searchRandom  element options = do
--    num <- newStdGen
--    let (x,_) = randomR(1,length(options)) num
--        (y,_) = randomR(1,length(element)) num

--    return $ list !! (x `mod` length(list))

searchBest :: [Int] -> [[Int]] -> [Int] -> [[Int]] -> [Int] -> Int -> [Int]
searchBest [] _ _ _ new_element _ = new_element
searchBest (x:rest) list options tabu new_element curr_sol = 
    let xs = rest ++ new_element
        l = [(y:xs) | y <- options, (filter_vector (y:xs) list) < curr_sol, (filter_vector (y:xs) tabu) < length(x:rest)]
    in case l of 
         [] -> searchBest rest list options tabu (x:new_element) curr_sol
         _ -> head(l) 



--ssl :: [[Int]] -> [[Int]] -> Int -> [[Int]]
--ssl (x:xs) tabu curr_sol =  


-- TODO
-- 2 algorithms: we stop when we cant find any better neighbors
-- random neighbors, even they are worse
