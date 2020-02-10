-- Attack of the Mixed Vigenere Cipher using Brute-force Frequency Analysis
-- This script is optimized for the brute-force

import Data.Char


-- Trying to crack this
-- Key is 6 digits
code :: String
code = "UNFDNKEPBXPXNMFIOWHMIDNHHETEJVUNYIVOEXUFOCWVMDZRTBRETEVXYENEGPFOVQTLFRCVPBVUNQGHYMQFEKUIOVPKYUVFTXOEVXMNAJMTCWOEZRWBXLQVUNFATOYNOLVXNQQDZRXBAQVFGNJIPCZHFUNFGHQHFXGFASUVPHODZRYBFFMHCEOOKWEFIIZWBLAJTRENBHSRCXDZJFCJBVUDVGOIUZQBBMURNCGONEUNFXVXWXEFZIEJYIUSVRQKOWIQRIBYNGNDZRDGOLVQPGVXVWEWBMUREQUSVNWBYMVCVLGMOVVCMFERNDJVMTCVAEOAVCULNWZVFFJFCJOLQVPHMJQTHXEOVCDKQFOCXGPXWQVWOLVKCKTMOPUGXVZXZCFDNYRXAOMTUGKECFOGQTNFEQQKMWEHOKJORLJVBQVGUDCFMVVHCFYDAZOPGKYFUCWWASUDVBERZOUKIIQCGCAVQFZNFMJZHFJVMTCHTDPSEHNIUCNRQQVFZGYVCHPWBVWRWFIIJHPKBOUECXNZMVCBJVXVPHODZRYBJJWOENBHLIUBFHJWEXFDBZNGOLVQUGEIQVPHQHJXSKELCHDHJHVFGQUDVQNGQVVVPQNXVDRPBXBXTJOESRNRULCFYCAZMTUGBCVFYKIMBZNGOLVQGJFFMTWXELCHGQUDVQGJJXZWEGYDZXUXJKZVWBJFOFCJJXLXUAIIUVPKGMJHUGGLVQNHWIOHXHRQMWZGYVVQCEAOMOEMOEZRNLBDZRNGQFPDUNFAUHCKQFOCPKBOUVPKQIGVDGYVCFYNFOIWDEBVKHCFAZMYVBNMBVJGAHQHUNFJCQGFYFKHVLOLVQTCJVXHHXXIJRGVBVMOEMOLJWHMIDZRSKBQVCNGAJBXEJRONWAKEFPDCDUEWVPKXIVUTXHDNKEIPUCCDGYVCFYHMMXTUBBMOZCMBVMWRHMSQHUNFAFRNKNFYOEMOLVONPBAOWNFIDZQVRHLQDWXEEWPNKFVWOCVEXQFZVJDMDCYJSPXYKUFBSCFOLVSPHEXVREXAXCPECAJWOYNOMOPXROFPDUNFEMTCBOOQJCVFOBXBGQKMTCBVDMRZBAFUHCKNIUVVIFKNOEMJVMTCGOLVQZQYIIVTGQFOCUNFRNECXJVMTCYJSPXYKUDZRQEBXBRZMBPVCWFOLVEJQOLFXNDFAVHWXEQVUUFIICQMNJSUQCXRSNHCLVOMTCBQEJVPFIIWOCVEXXXIKXFKVVBASPOEMIMPDGHQHMTTALKJWIKUEWWBJGEJRGFOLVQVHEHFOEJMIUVVHOOQNAHQHEOBVBKVHXKRFTRBKUXIWDWAV"
code' :: [Int]
code' = [20,13,5,3,13,10,4,15,1,23,15,23,13,12,5,8,14,22,7,12,8,3,13,7,7,4,19,4,9,21,20,13,24,8,21,14,4,23,20,5,14,2,22,21,12,3,25,17,19,1,17,4,19,4,21,23,24,4,13,4,6,15,5,14,21,16,19,11,5,17,2,21,15,1,21,20,13,16,6,7,24,12,16,5,4,10,20,8,14,21,15,10,24,20,21,5,19,23,14,4,21,23,12,13,0,9,12,19,2,22,14,4,25,17,22,1,23,11,16,21,20,13,5,0,19,14,24,13,14,11,21,23,13,16,16,3,25,17,23,1,0,16,21,5,6,13,9,8,15,2,25,7,5,20,13,5,6,7,16,7,5,23,6,5,0,18,20,21,15,7,14,3,25,17,24,1,5,5,12,7,2,4,14,14,10,22,4,5,8,8,25,22,1,11,0,9,19,17,4,13,1,7,18,17,2,23,3,25,9,5,2,9,1,21,20,3,21,6,14,8,20,25,16,1,1,12,20,17,13,2,6,14,13,4,20,13,5,23,21,23,22,23,4,5,25,8,4,9,24,8,20,18,21,17,16,10,14,22,8,16,17,8,1,24,13,6,13,3,25,17,3,6,14,11,21,16,15,6,21,23,21,22,4,22,1,12,20,17,4,16,20,18,21,13,22,1,24,12,21,2,21,11,6,12,14,21,21,2,12,5,4,17,13,3,9,21,12,19,2,21,0,4,14,0,21,2,20,11,13,22,25,21,5,5,9,5,2,9,14,11,16,21,15,7,12,9,16,19,7,23,4,14,21,2,3,10,16,5,14,2,23,6,15,23,22,16,21,22,14,11,21,10,2,10,19,12,14,15,20,6,23,21,25,23,25,2,5,3,13,24,17,23,0,14,12,19,20,6,10,4,2,5,14,6,16,19,13,5,4,16,16,10,12,22,4,7,14,10,9,14,17,11,9,21,1,16,21,6,20,3,2,5,12,21,21,7,2,5,24,3,0,25,14,15,6,10,24,5,20,2,22,22,0,18,20,3,21,1,4,17,25,14,20,10,8,8,16,2,6,2,0,21,16,5,25,13,5,12,9,25,7,5,9,21,12,19,2,7,19,3,15,18,4,7,13,8,20,2,13,17,16,16,21,5,25,6,24,21,2,7,15,22,1,21,22,17,22,5,8,8,9,7,15,10,1,14,20,4,2,23,13,25,12,21,2,1,9,21,23,21,15,7,14,3,25,17,24,1,9,9,22,14,4,13,1,7,11,8,20,1,5,7,9,22,4,23,5,3,1,25,13,6,14,11,21,16,20,6,4,8,16,21,15,7,16,7,9,23,18,10,4,11,2,7,3,7,9,7,21,5,6,16,20,3,21,16,13,6,16,21,21,21,15,16,13,23,21,3,17,15,1,23,1,23,19,9,14,4,18,17,13,17,20,11,2,5,24,2,0,25,12,19,20,6,1,2,21,5,24,10,8,12,1,25,13,6,14,11,21,16,6,9,5,5,12,19,22,23,4,11,2,7,6,16,20,3,21,16,6,9,9,23,25,22,4,6,24,3,25,23,20,23,9,10,25,21,22,1,9,5,14,5,2,9,9,23,11,23,20,0,8,8,20,21,15,10,6,12,9,7,20,6,6,11,21,16,13,7,22,8,14,7,23,7,17,16,12,22,25,6,24,21,21,16,2,4,0,14,12,14,4,12,14,4,25,17,13,11,1,3,25,17,13,6,16,5,15,3,20,13,5,0,20,7,2,10,16,5,14,2,15,10,1,14,20,21,15,10,16,8,6,21,3,6,24,21,2,5,24,13,5,14,8,22,3,4,1,21,10,7,2,5,0,25,12,24,21,1,13,12,1,21,9,6,0,7,16,7,20,13,5,9,2,16,6,5,24,5,10,7,21,11,14,11,21,16,19,2,9,21,23,7,7,23,23,8,9,17,6,21,1,21,12,14,4,12,14,11,9,22,7,12,8,3,25,17,18,10,1,16,21,2,13,6,0,9,1,23,4,9,17,14,13,22,0,10,4,5,15,3,2,3,20,4,22,21,15,10,23,8,21,20,19,23,7,3,13,10,4,8,15,20,2,2,3,6,24,21,2,5,24,7,12,12,23,19,20,1,1,12,14,25,2,12,1,21,12,22,17,7,12,18,16,7,20,13,5,0,5,17,13,10,13,5,24,14,4,12,14,11,21,14,13,15,1,0,14,22,13,5,8,3,25,16,21,17,7,11,16,3,22,23,4,4,22,15,13,10,5,21,22,14,2,21,4,23,16,5,25,21,9,3,12,3,2,24,9,18,15,23,24,10,20,5,1,18,2,5,14,11,21,18,15,7,4,23,21,17,4,23,0,23,2,15,4,2,0,9,22,14,24,13,14,12,14,15,23,17,14,5,15,3,20,13,5,4,12,19,2,1,14,14,16,9,2,21,5,14,1,23,1,6,16,10,12,19,2,1,21,3,12,17,25,1,0,5,20,7,2,10,13,8,20,21,21,8,5,10,13,14,4,12,9,21,12,19,2,6,14,11,21,16,25,16,24,8,8,21,19,6,16,5,14,2,20,13,5,17,13,4,2,23,9,21,12,19,2,24,9,18,15,23,24,10,20,3,25,17,16,4,1,23,1,17,25,12,1,15,21,2,22,5,14,11,21,4,9,16,14,11,5,23,13,3,5,0,21,7,22,23,4,16,21,20,20,5,8,8,2,16,12,13,9,18,20,16,2,23,17,18,13,7,2,11,21,14,12,19,2,1,16,4,9,21,15,5,8,8,22,14,2,21,4,23,23,23,8,10,23,5,10,21,21,1,0,18,15,14,4,12,8,12,15,3,6,7,16,7,12,19,19,0,11,10,9,22,8,10,20,4,22,22,1,9,6,4,9,17,6,5,14,11,21,16,21,7,4,7,5,14,4,9,12,8,20,21,21,7,14,14,16,13,0,7,16,7,4,14,1,21,1,10,21,7,23,10,17,5,19,17,1,10,20,23,8,22,3,22,0,21]

example :: String
-- example = "ASHEPULLEDINTOTHEPARKINGSLOTNEARTHEEDGEOFTHEHUGEASPHALTLOTANEMPTYBEERCANCRUNCHEDUNDERONEOFTHEFRONTWHEELSOFTHECARHETURNEDOFFHISLIGHTSANDSURVEYEDTHEAREAYESTHISWASAGOODSPOTHEHADACLEARVIEWOFEACHAUTOMOBILETURNINGFROMTHELONEENTRANCEDRIVEWAYINTOTHELOTWHEREITHADTOSLOWALMOSTTOASTOPUNDERTHEBRIGHTMERCURYVAPORLAMPTHEREANDHEALSOWASWELLSITUATEDFORSEEINGWHICHROWOFTHELOTEACHVEHICLEEVENTUALLYTURNEDINTOHEPULLEDHISCOATMORESNUGLYAROUNDHISNECKTURNEDTHERADIODIALUNTILHEFOUNDANFMSTATIONWHICHWASBROADCASTINGHISFAVORITESCHUBERTSONATAANDSETTLEDDOWNTOWAIT"
example = "IVCNSRUOYMLJARQQGMIUGRQBYOLAQYIUQQGYMIYXHQQGCBIYIVMQBHAOLABJNPMAYZNGOLBJLURWDCNFRWFYERJNRAAJYOULWWTQGYUVLOWCNDXEJYAXOWGEXHAQLPULBQWPIQEYXOCGVNFQQGXEGXGGPAJDYKXYBBXREYSLAJYQBEIDHNBOCLYDRANBKQBRARIXCDUGQBUJRQBOULVWCNOLWGYWWOIQKNFORZYDBVRQQXWCNOLAKCNUYRWCIFQXVHXKXUPLYWQXBPARMBQENUQQGZELBQWINUKBUVCBMXUHIPMAJYEGXWFCNBHYRTIVTNOHYLQBBQNFAXUPNGDWITQLKQULDRAAJYURQNBKQZYQLKUGYCGJAXXUOVAXOWGERQQXJYZXHUGEQLPLRXAPLEGPWXBUYXERRWFCRVJNDGAXOWGEAJYEBERRERBHBQQROCNHLBQEIQAVVQIWDXQTQLKQKXYCOXBELBPALJPJDYHXCRORWYYDCBCYEWPXQXABXWFPNWQUGEMRTWWLDBDA"
example' :: [Int]
example' = [8,21,2,13,18,17,20,14,24,12,11,9,0,17,16,16,6,12,8,20,6,17,16,1,24,14,11,0,16,24,8,20,16,16,6,24,12,8,24,23,7,16,16,6,2,1,8,24,8,21,12,16,1,7,0,14,11,0,1,9,13,15,12,0,24,25,13,6,14,11,1,9,11,20,17,22,3,2,13,5,17,22,5,24,4,17,9,13,17,0,0,9,24,14,20,11,22,22,19,16,6,24,20,21,11,14,22,2,13,3,23,4,9,24,0,23,14,22,6,4,23,7,0,16,11,15,20,11,1,16,22,15,8,16,4,24,23,14,2,6,21,13,5,16,16,6,23,4,6,23,6,6,15,0,9,3,24,10,23,24,1,1,23,17,4,24,18,11,0,9,24,16,1,4,8,3,7,13,1,14,2,11,24,3,17,0,13,1,10,16,1,17,0,17,8,23,2,3,20,6,16,1,20,9,17,16,1,14,20,11,21,22,2,13,14,11,22,6,24,22,22,14,8,16,10,13,5,14,17,25,24,3,1,21,17,16,16,23,22,2,13,14,11,0,10,2,13,20,24,17,22,2,8,5,16,23,21,7,23,10,23,20,15,11,24,22,16,23,1,15,0,17,12,1,16,4,13,20,16,16,6,25,4,11,1,16,22,8,13,20,10,1,20,21,2,1,12,23,20,7,8,15,12,0,9,24,4,6,23,22,5,2,13,1,7,24,17,19,8,21,19,13,14,7,24,11,16,1,1,16,13,5,0,23,20,15,13,6,3,22,8,19,16,11,10,16,20,11,3,17,0,0,9,24,20,17,16,13,1,10,16,25,24,16,11,10,20,6,24,2,6,9,0,23,23,20,14,21,0,23,14,22,6,4,17,16,16,23,9,24,25,23,7,20,6,4,16,11,15,11,17,23,0,15,11,4,6,15,22,23,1,20,24,23,4,17,17,22,5,2,17,21,9,13,3,6,0,23,14,22,6,4,0,9,24,4,1,4,17,17,4,17,1,7,1,16,16,17,14,2,13,7,11,1,16,4,8,16,0,21,21,16,8,22,3,23,16,19,16,11,10,16,10,23,24,2,14,23,1,4,11,1,15,0,11,9,15,9,3,24,7,23,2,17,14,17,22,24,24,3,2,1,2,24,4,22,15,23,16,23,0,1,23,22,5,15,13,22,16,20,6,4,12,17,19,22,22,11,3,1,3,0]


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

-- Adds spaces to a message according to the key length
format_read :: String -> Int -> String
format_read [] _ = []
format_read xs n = take n xs' ++ " " ++ format_read (drop n xs') n
    where 
        xs' = format xs

-- Formats a message for for a faster attack
format_attack :: String -> [Int]
format_attack xs = map chr2int (format xs)

attack2read :: [Int] -> String
attack2read xs = map int2chr xs

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

-- The attack itself, given a key length y on a message xs
attack_test :: [Int] -> Int -> [([Int],Float)]
attack_test xs y = [(y', z) | (y', z) <- zip ys zs, z < 100]
    where 
        ys = gen_keys y
        zs = map ((chisqr target_freqs) . freqs . decode xs) ys