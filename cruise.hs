module Cruise where
import Data.List 

main :: IO()
main = do
  putStrLn "Please enter the following parameters: \n number of diners, "
  diners <- getLine
  putStrLn "number of chairs.. \n"
  chairs <- getLine
  putStrLn " we are almost done! Now please enter the number of evenings "
  evenings <- getLine
  putStrLn "Your program is ready!"
  writeFile "cruise_output.txt" "aaa"
  --putStrLn "c Here is a comment"
  --putStrLn "p cnf" ++ (length (set_variables diners evenings)) ++ (length (ans diners chairs evenings))


--to_cnf_text :: Int -> Int -> Int -> Int
--to_cnf_text diners chairs evenings = 
--  putStrLn "c Here is a comment "
--  putStrLn "p cnf " ++ (length (set_variables diners evenings)) ++ (length (ans diners chairs evenings))

to_cnf_text :: Int -> Int -> Int -> String
to_cnf_text diners chairs evenings =
  let anss = (ans diners chairs evenings) 
      vars = length (set_variables diners evenings)
    in  ("p cnf " ++ show vars ++ " " ++ show (length anss) ++
      "\n" ) --TODO
    

ans :: Int -> Int -> Int -> [[Int]]
ans diners chairs evenings =
  let tables = diners `div` chairs 
      tabu_combs = oh_no_please diners
      bits = (ceiling (logBase (fromIntegral 2) (fromIntegral diners)))
      list = set_variables diners evenings
      big_list = structure_4list list bits (bits*chairs) tables
      oh_no_cnf = oh_no_please_cnf (concat(concat big_list)) diners -- ADD
      all_combs = delete_many (oh_no_please diners) (mapM (const [1,-1]) [1..bits]) 
      cnf_diff = all_diff_cnf big_list all_combs -- ADD
      cnf_intersect = not_two_in_common big_list all_combs -- ADD
    in (oh_no_cnf ++ cnf_diff ++ cnf_intersect) 


not_two_in_common :: [[[[ Int ]]]] -> [[Int]] -> [[Int]]
not_two_in_common [] _ = []
not_two_in_common (x:xs) combinations = 
  let xx2 = concat [ subs 2 xx | xx <- x]
      c_xs = concat xs
      xxs2 = concat [ subs 2 xxs | xxs <- c_xs]
      in (concat [ mix_2x2 (xx++xxs) combinations |  xx <- xx2, xxs <- xxs2 ] ++
      (not_two_in_common xs combinations))

set_variables :: Int -> Int -> [Int]
set_variables diners evenings = 
  let total = (ceiling (logBase (fromIntegral 2) (fromIntegral diners) )) * diners * evenings
    in [1..total] 

subs :: Int -> [[Int]] -> [[[Int]]]
subs x = filter ((== x) . length) . subsequences

structure_4list :: [Int]  -> Int -> Int -> Int -> [[[[Int]]]]
structure_4list list diners tables evenings = 
  convert_to4 (map (slice diners) (slice tables list)) evenings

oh_no_please_cnf :: [[Int]] -> Int -> [[Int]]
oh_no_please_cnf [] _ = []
oh_no_please_cnf (x:xs) diners = 
  [ zipWith(*) x y | y <- (oh_no_please diners)] ++ (oh_no_please_cnf xs diners)

all_diff_cnf :: [[[[ Int ]]]] -> [[Int]] -> [[Int]]
all_diff_cnf [] _ = []
all_diff_cnf (x:xs) combs =
  (all_different (concat x) combs)  ++ all_diff_cnf xs combs

all_different :: [[Int]] -> [[Int]] -> [[Int]]
all_different [] _ = []
all_different (x:rest) combinations =
 ( [  zipWith (*) (comb++comb) (x++xs) | xs<-rest, comb <- combinations])++
 all_different rest combinations

oh_no_please :: Int -> [[Int]]
oh_no_please diners = 
  let limit = if (diners<5) then 3 else if (diners<9) then 7 else if (diners<17) then 15 else if (diners<33) then 31 else if (diners<65) then 63 else if (diners < 129) then 127 else 255
    in (map convert' [diners..limit] )

convert' :: Int -> [Int]
convert' num = reverse (convert num)

convert :: Int -> [Int]
convert 1 = [-1]
convert 0 = [1]
convert num = let
  d = num `div` 2 
  r = num `rem` 2
  ans = if (r==0) then 1 else -1
    in [ans] ++ (convert d) 

slice :: Int -> [Int] -> [[Int]]
slice _ [] = []
slice index list = (take index list):(slice index (drop index list))

convert_to4 :: [[[Int]]] -> Int -> [[[[Int]]]]
convert_to4 [] _ = []
convert_to4 list index =  (take index list):(convert_to4 (drop index list) index)

-- If two of them are equal, the other two must be different
mix_2x2 :: [[Int]] -> [[Int]] -> [[Int]]
mix_2x2 _ [] = []
mix_2x2 [x1,x2,x3,x4] (possibility1:rest_of_possibilities) =
  [ (zipWith (*)  (possibility1 ++ possibility1) (x1++x3)) ++
    (zipWith (*)  (xs ++ xs) (x2++x4) )| xs <- rest_of_possibilities ]  ++
    [ (zipWith (*)  (possibility1 ++ possibility1) (x1++x4)) ++
    (zipWith (*)  (xs ++ xs) (x2++x3) )| xs <- rest_of_possibilities ] ++  
    mix_2x2 [x1,x2,x3,x4] rest_of_possibilities
   
delete_many :: Eq a => [a] -> [a] -> [a]
delete_many [] list = list
delete_many (x:xs) list = 
  delete_many xs (delete x list)
