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

-- This is some kind of dotproduct
dotproduct :: [Int] -> [Int] -> Int
dotproduct list1 list2 = length $ list1 `intersect` list2

delete_lists :: (Eq a) => [a] -> [a] -> [a]
delete_lists [] list = list
delete_lists (x:xs) big_list = delete_lists xs $ delete x big_list 

worst_ones :: [[Int]] -> Int -> [[Int]]
worst_ones [x] _ = []
worst_ones (x:xs) limit = 
    if ( (filter_vector x xs) >= limit) then (x:(worst_ones xs limit))
        else (worst_ones xs limit)

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

localID :: Int -> Int -> Int -> IO Int
localID rows columns r = do 
    first_solution <- (randlists rows columns r)
    ans <- mainf first_solution [[]] rows columns r (filter_dotprod first_solution) (filter_dotprod first_solution) 30
    return $ ans

mainf :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int 
mainf _ _ _ _ _ _ best_sol 0 = return best_sol
mainf big_list tabu rows columns r curr_sol best_sol counter = 
    let changes = worst_ones big_list curr_sol
        new_elements = neighbors changes big_list tabu columns curr_sol
        new_list = (delete_lists changes big_list) ++ new_elements
        new_best = minimum [(filter_dotprod new_list),best_sol]
        in do 
            reroll <- shuffle new_list columns curr_sol
            shuffle_ans <- mainf reroll (tabu++new_elements) rows columns r (filter_dotprod reroll) new_best (counter-1)
            return shuffle_ans
 
shuffle :: [[Int]] -> Int -> Int -> IO [[Int]]
shuffle big_list columns worst_sol = 
    let changes = (worst_ones big_list worst_sol)
        non_modified = delete_lists changes big_list
        in do  
            modifications <- randlists (length(changes)) columns (length(head(big_list)))
            return (modifications++non_modified)

neighbors :: [[Int]] -> [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
neighbors [] list tabu _ curr_sol = []
neighbors (element:rest) list tabu limit curr_sol =
    let (x:xs) = element
        list2 = delete element list 
        options = delete_lists (x:xs) [1..limit] 
        ans = searchBest element list2 options limit tabu [] curr_sol
    in (ans: neighbors rest (ans:list2) (ans:tabu) limit curr_sol)

searchBest :: [Int] -> [[Int]] -> [Int] -> Int -> [[Int]] -> [Int] -> Int -> [Int]
searchBest [] _ _ _ _ new_element _ = new_element
searchBest (x:rest) list options limit tabu new_element curr_sol = 
    let xs = rest ++ new_element
        new_options = closest (x:rest) options tabu limit
        l = [ y | y <- new_options, (filter_vector (y:xs) list) < curr_sol]
    in case l of 
         [] -> searchBest rest list options limit tabu (x:new_element) curr_sol
         _ -> (head(l):xs)

closest ::  [Int] -> [Int] -> [[Int]] -> Int ->[Int]
closest (num:xs) list tabu limit =    
    let lmin = [x | x <- (reverse([1..(num-1)] `intersect` list)), (filter_vector (x:xs) tabu) < (length(num:xs)) ]
        lmax = [x | x <- ([(num+1)..limit] `intersect` list), (filter_vector (x:xs) tabu) < (length(num:xs)) ] 
        minim = if (lmin == []) then [] else (take 2(lmin))
        maxim = if (lmax == []) then [] else (take 2(lmax))
    in  (minim++maxim)
