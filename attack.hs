-- Attack of the Mixed Vigenere Cipher using Brute-force Frequency Analysis
-- This script is optimized for the brute-force

import Data.Char

-- Original text: "As he pulled into the parking slot near the edge of the huge, asphalt lot an empty beer can crunched under one of the front wheels of the car. He turned off his lights and surveyed the area. Yes, this was a good spot; he had a clear view of each automobile turning from the lone entrance driveway into the lot, where it had to slow almost to a stop under the bright, mercury-vapor lamp there, and he also was well situated for seeing which row of the lot each vehicle eventually turned into. He pulled his coat more snugly around his neck, turned the radio dial until he found an FM station which was broadcasting his favorite Schubert sonata, and settled down to wait."
example_text :: String
example_text = "ASHEPULLEDINTOTHEPARKINGSLOTNEARTHEEDGEOFTHEHUGEASPHALTLOTANEMPTYBEERCANCRUNCHEDUNDERONEOFTHEFRONTWHEELSOFTHECARHETURNEDOFFHISLIGHTSANDSURVEYEDTHEAREAYESTHISWASAGOODSPOTHEHADACLEARVIEWOFEACHAUTOMOBILETURNINGFROMTHELONEENTRANCEDRIVEWAYINTOTHELOTWHEREITHADTOSLOWALMOSTTOASTOPUNDERTHEBRIGHTMERCURYVAPORLAMPTHEREANDHEALSOWASWELLSITUATEDFORSEEINGWHICHROWOFTHELOTEACHVEHICLEEVENTUALLYTURNEDINTOHEPULLEDHISCOATMORESNUGLYAROUNDHISNECKTURNEDTHERADIODIALUNTILHEFOUNDANFMSTATIONWHICHWASBROADCASTINGHISFAVORITESCHUBERTSONATAANDSETTLEDDOWNTOWAIT"


-- General Functions

chr2int :: Char -> Int
chr2int c = ord c - ord 'A'

int2chr :: Int -> Char
int2chr n = chr (ord 'A' + n)


-- Sets a message to all capital letters
format :: String -> String
format [] = []
format (x:xs)
    | isUpper x = [x] ++ format xs
    | isLower x = [chr (ord x - ord 'a' + ord 'A')] ++ format xs
    | isDigit x = [chr (ord 'A' + (mod (chr2int x) 26))] ++ format xs
    | otherwise = format xs

-- Formats a message for for a faster attack
format_attack :: String -> [Int]
format_attack xs = map chr2int (format xs)

format_read :: [Int] -> String
format_read xs = map int2chr xs

-- Returns the positions of an element
positions :: Eq a => [a] -> a -> [Int]
positions xs y = [i | (x, i) <- zip xs [0..n], x == y]
    where
        n = length xs - 1

-- Returns the first position of an element
position :: Eq a => [a] -> a -> Int
position xs x = head (positions xs x)

-- Count occurrences of an element
count :: Eq a => [a] -> a -> Int
count xs x = length (positions xs x)

find :: Eq a => [a] -> a -> Bool
find xs x = not (positions xs x == [])

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: [Int] -> [Float]
freqs xs = [percent (count xs x) (length xs) | x <- [0..25]]

-- Removes Duplicates
remove_dup :: Eq a => [a] -> [a]
remove_dup xs = [x | (x, i) <- zip xs [0..n], not (find (take i xs) x)]
    where
        n = length xs - 1

-- Chi-Square Test
chisqr :: [Float] -> [Float] -> Float
chisqr target xs = sum [((x - y) ^ 2) / y | (x, y) <- zip xs target]

target_freqs :: [Float]
target_freqs = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,
    6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]

shift :: Int -> Int -> Int
shift x y = (x + y) `mod` 26


-- Vigenere, Normal Operations

-- Encodes xs using ys using Simple Vigenere
encode_simple :: [Int] -> [Int] -> [Int]
encode_simple xs ys = [shift x y | (x, y) <- zip xs ys']
    where
        ys' = ys ++ ys'

-- Decodes xs using ys using Simple Vigenere
decode_simple :: [Int] -> [Int] -> [Int]
decode_simple xs ys = [shift x (-y) | (x, y) <- zip xs ys']
    where
        ys' = ys ++ ys'
        
gen_alphabet :: [Int] -> [Int]
gen_alphabet ys = ys' ++ [y | y <- [0..25], not (find ys' y)]
    where
        ys' = remove_dup ys

-- Encodes xs using ys using Mixed Vigenere
encode :: [Int] -> [Int] -> [Int]
encode xs ys = [alphabet!!x | x <- xs']
    where 
        alphabet = gen_alphabet ys
        xs' = encode_simple xs ys

-- Decodes xs using ys using Mixed Vigenere
decode :: [Int] -> [Int] -> [Int]
decode xs ys = decode_simple xs' ys
    where 
        alphabet = gen_alphabet ys
        xs' = map (position alphabet) xs


-- Brute-force Attack

-- Generates all keys of length n
gen_keys :: Int -> [[Int]]
gen_keys n
    | n <= 0 = []
    | n == 1 = [[x] | x <- [0..25]]
    | otherwise = [[x] ++ y | x <- [0..25], y <- ys]
        where 
            ys = gen_keys (n-1)

-- The attack itself, given a key length y on a message xs
attack :: [Int] -> Int -> ([Int],Float)
attack xs y = (ys!!i, zs!!i)
    where 
        ys = gen_keys y
        zs = map ((chisqr target_freqs) . freqs . decode xs) ys
        i = position zs (minimum zs)


-- Ignore
-- attack_test3 :: [Int] -> Int -> [(Float, ([Int], [Int]))]
-- attack_test3 xs y = [(z, yx) | (z, yx) <- zip zs yxs, z < 100]
-- -- attack_test3 xs y = [(z, yx) | (z, yx) <- zip zs yxs]
--     where 
--         ys = drop (26^5) (gen_keys y)
--         -- ys = take (26^4) (gen_keys y)
--         xs' = [decode xs y | y <- ys]
--         yxs = [(ys!!i, xs'!!i) | i <- [0..], (xs'!!i)!!0 == 4, (xs'!!i)!!2 == 4, (xs'!!i)!!18 == 4, (xs'!!i)!!32 == 4]
--         zs = map ((chisqr target_freqs) . freqs) (map snd yxs)

-- -- attack_test3 (drop (1331 - 51) code') 6