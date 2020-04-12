-- Name : Tariq Aboelyazid
-- ID : 18024368
import Data.Char

chomp :: String -> String
chomp [] = [] -- empty list return empty list
chomp (x:xs) = x : takeWhile (== x) xs


munch :: String -> String
munch xs = take 9 (chomp xs)

runs :: String -> [String]
runs [] = []
runs ns = xs : runs ys
 where
  xs = munch ns
  ys = drop (length xs) ns 

encode :: String -> [(Char,Int)]
encode ns = [(head xs, length xs) | xs <- runs ns]

flatten :: [(Char,Int)] -> String
flatten [] = []
flatten ((x,y):xs) = [x] ++ show y ++ flatten xs

compress :: String -> String
compress ns = flatten (encode ns)

decode :: [(Char,Int)] -> String
decode [] = []
decode ((x,y):xs) = [ns | ns <- replicate y x] ++ decode xs

expand :: String -> [(Char,Int)]
expand [] = []
expand (x:y:ys) = [(x,digitToInt y)] ++ expand ys

decompress :: String -> String
decompress ns = decode (expand ns)