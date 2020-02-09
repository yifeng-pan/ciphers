import Data.Char

-- Trying to crack this
-- Key is 6 digits
code :: String
code = "UNFDNKEPBXPXNMFIOWHMIDNHHETEJVUNYIVOEXUFOCWVMDZRTBRETEVXYENEGPFOVQTLFRCVPBVUNQGHYMQFEKUIOVPKYUVFTXOEVXMNAJMTCWOEZRWBXLQVUNFATOYNOLVXNQQDZRXBAQVFGNJIPCZHFUNFGHQHFXGFASUVPHODZRYBFFMHCEOOKWEFIIZWBLAJTRENBHSRCXDZJFCJBVUDVGOIUZQBBMURNCGONEUNFXVXWXEFZIEJYIUSVRQKOWIQRIBYNGNDZRDGOLVQPGVXVWEWBMUREQUSVNWBYMVCVLGMOVVCMFERNDJVMTCVAEOAVCULNWZVFFJFCJOLQVPHMJQTHXEOVCDKQFOCXGPXWQVWOLVKCKTMOPUGXVZXZCFDNYRXAOMTUGKECFOGQTNFEQQKMWEHOKJORLJVBQVGUDCFMVVHCFYDAZOPGKYFUCWWASUDVBERZOUKIIQCGCAVQFZNFMJZHFJVMTCHTDPSEHNIUCNRQQVFZGYVCHPWBVWRWFIIJHPKBOUECXNZMVCBJVXVPHODZRYBJJWOENBHLIUBFHJWEXFDBZNGOLVQUGEIQVPHQHJXSKELCHDHJHVFGQUDVQNGQVVVPQNXVDRPBXBXTJOESRNRULCFYCAZMTUGBCVFYKIMBZNGOLVQGJFFMTWXELCHGQUDVQGJJXZWEGYDZXUXJKZVWBJFOFCJJXLXUAIIUVPKGMJHUGGLVQNHWIOHXHRQMWZGYVVQCEAOMOEMOEZRNLBDZRNGQFPDUNFAUHCKQFOCPKBOUVPKQIGVDGYVCFYNFOIWDEBVKHCFAZMYVBNMBVJGAHQHUNFJCQGFYFKHVLOLVQTCJVXHHXXIJRGVBVMOEMOLJWHMIDZRSKBQVCNGAJBXEJRONWAKEFPDCDUEWVPKXIVUTXHDNKEIPUCCDGYVCFYHMMXTUBBMOZCMBVMWRHMSQHUNFAFRNKNFYOEMOLVONPBAOWNFIDZQVRHLQDWXEEWPNKFVWOCVEXQFZVJDMDCYJSPXYKUFBSCFOLVSPHEXVREXAXCPECAJWOYNOMOPXROFPDUNFEMTCBOOQJCVFOBXBGQKMTCBVDMRZBAFUHCKNIUVVIFKNOEMJVMTCGOLVQZQYIIVTGQFOCUNFRNECXJVMTCYJSPXYKUDZRQEBXBRZMBPVCWFOLVEJQOLFXNDFAVHWXEQVUUFIICQMNJSUQCXRSNHCLVOMTCBQEJVPFIIWOCVEXXXIKXFKVVBASPOEMIMPDGHQHMTTALKJWIKUEWWBJGEJRGFOLVQVHEHFOEJMIUVVHOOQNAHQHEOBVBKVHXKRFTRBKUXIWDWAV"


-- General Functions

-- Sets a message to the correct format for this program
format :: String -> String
format [] = []
format (x:xs)
    | isUpper x = [x] ++ format xs
    | isLower x = [chr (ord x - ord 'a' + ord 'A')] ++ format xs
    | otherwise = format xs

-- Returns the positions of an element
positions :: Eq a => [a] -> a -> [Int]
positions xs target = [i | (x, i) <- zip xs [0..(length xs - 1)], x == target]

-- Returns the first position of an element
position :: Eq a => [a] -> a -> Int
position xs target = head (positions xs target)

chr2int :: Char -> Int
chr2int c = ord c - ord 'A'

int2chr :: Int -> Char
-- int2chr n = chr ((ord 'A' + n) `mod` 26 + ord 'A')
int2chr n = chr (ord 'A' + n)

shift :: Char -> Int -> Char
shift c n
    | isUpper c = int2chr((chr2int c + n) `mod` 26)
    | otherwise = c

-- Second part of shift used for the Mixed Vigenère
shift2 :: Char -> [Char] -> Char
shift2 c alphabet
    | isUpper c =int2chr (position alphabet c)
    | otherwise = c

remove_dup :: String -> String
remove_dup xs = [x | (x, i) <- zip xs [0..(length xs - 1)], positions (take i xs) x == []]


-- Vigenere

-- Encodes xs using ys using Simple Vigenere
encode_simple :: String -> String -> String
encode_simple xs ys = [shift x (key!!(i `mod` length key)) | (x, i) <- zip xs [0..(length xs - 1)]]
    where 
        key = map chr2int ys

-- Encodes xs using ys using Mixed Vigenere
encode :: String -> String -> String
encode xs ys = [alphabet!!(chr2int x) | x <- xs']
    where 
        alphabet = gen_alphabet ys
        xs' = encode_simple xs ys

decode :: String -> String -> String
decode xs ys = encode_simple xs' ys'
    where 
        alphabet = gen_alphabet ys
        xs' = map (int2chr . position alphabet) xs
        ys' = [int2chr (26 - (chr2int y) `mod` 26) | y <- ys]

default_alphabet :: [Char]
default_alphabet = map int2chr [0..25]

gen_alphabet :: String -> [Char]
gen_alphabet xs = mod_key ++ [int2chr x | x <- [0..25], positions mod_key (int2chr x) == []]
    where
        mod_key = remove_dup xs


-- Brute-force Frequency Analysis for Mixed

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- Count occurrences of a char
count :: String -> Char -> Int
count xs target = length [x | x <- xs, x == target]

-- Chi-Square Test
chisqr :: [Float] -> [Float] -> Float
chisqr target xs = sum [((x - y) ^ 2) / y | (x, y) <- zip xs target]

target_freqs :: [Float]
target_freqs = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,
    6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]

freqs :: String -> [Float]
freqs xs = [percent (count xs x) (length xs) | x <- ['A'..'Z']]

-- BAD
-- Generates all keys of length n
gen_keys :: Int -> [String]
gen_keys n 
    | n == 1 = [[int2chr a] | a <- [0..25]]
    | n == 2 = [[int2chr a] ++ [int2chr b] | a <- [0..25], b <- [0..25]]
    | n == 3 = [[int2chr a] ++ [int2chr b] ++ [int2chr c]  | a <- [0..25], b <- [0..25], c <- [0..25]]
    | n == 4 = [[int2chr a] ++ [int2chr b] ++ [int2chr c] ++ [int2chr d] | a <- [0..25], b <- [0..25], c <- [0..25], d <- [0..25]]
    | n == 5 = [[int2chr a] ++ [int2chr b] ++ [int2chr c] ++ [int2chr d] ++ [int2chr e] | a <- [0..25], b <- [0..25], c <- [0..25], d <- [0..25], e <- [0..25]]
    | n == 6 = [[int2chr a] ++ [int2chr b] ++ [int2chr c] ++ [int2chr d] ++ [int2chr e] ++ [int2chr f] | a <- [0..25], b <- [0..25], c <- [0..25], d <- [0..25], e <- [0..25], f <- [0..25]]
    | otherwise = []

attack :: String -> Int -> (String,Float)
attack xs key_length = (keys!!i, chis!!i)
    where 
        keys = gen_keys key_length
        chis = map ((chisqr target_freqs) . freqs . decode xs) keys
        i = position chis (minimum chis)

attack_test :: String -> Int -> [(String,Float)]
attack_test xs key_length = [ (k, c) | (k, c) <- zip keys chis, c < 200]
    where 
        keys = gen_keys key_length
        chis = map ((chisqr target_freqs) . freqs . decode xs) keys

attack_test2 :: String -> Int -> (String,Float)
attack_test2 xs key_length = (keys!!i, chis!!i)
    where 
        keys = gen_keys key_length
        chis = map ((chisqr target_freqs) . freqs . decode xs) keys
        chis' = take 1000 chis
        i = position chis' (minimum chis')


-- For Analysis by hand

-- Breakdown Vigenere into Caesar
caesar :: String -> Int -> Int -> String
caesar xs size offset = [x | (x, i) <- zip xs [0..(length xs - 1)], i `mod` size == offset]

freqs2 :: String -> [(Char, Float)]
freqs2 xs = [(x, percent (count xs x) (length xs)) | x <- ['A'..'Z']]

qsort :: [(Char, Float)] -> [(Char, Float)]
qsort []     = []
qsort (p:xs) = (qsort greater) ++ [p] ++ (qsort lesser)
    where
        lesser  = [x | x <- xs, snd x < snd p]
        greater = [x | x <- xs, snd x >= snd p]

-- Frequency Analysis check to get a lookup table
check :: String -> Int -> Int -> [(Char, Float)]
check xs size offset = take 5 (qsort (freqs2 (caesar xs size offset)))

guess_alphabet :: [Char]
guess_alphabet = [
    'A', '_', 'H', '_', 'O', 'W',
    'N', 'K', 'Z', 'R', 'C', '_',
    'X', 'M', 'B', '_', '_', '_', 
    'G', 'F', 'U', '_', '_', '_', 
    'V', 'E']

guess_key :: String
guess_key = "UMABGV"

guess :: String -> String
guess xs = [guess_alphabet!!((chr2int x - chr2int (guess_key!!(i `mod` length guess_key))) `mod` 26) | (x,i) <- zip xs [0..l]]
    where
        l = length xs - 1

guess_with_key :: String -> String -> String
guess_with_key xs key = [guess_alphabet!!((chr2int x - chr2int (key!!(i `mod` length key))) `mod` 26) | (x,i) <- zip xs [0..l]]
    where
        l = length xs - 1

-- Temp
format2 :: String -> String
format2 [] = []
format2 xs = take 6 xs ++ " " ++ format2 (drop 6 xs)

guess' :: String -> String
guess' xs = format2 (guess xs)

guess_with_key' :: String -> String -> String
guess_with_key' xs key = format2 (guess_with_key xs key)


compare_guess :: String -> String -> String -> String
compare_guess xs key1 key2 = [if a == b then a else '_' | (a, b) <- zip guess1 guess2]
    where 
        guess1 = guess_with_key xs key1
        guess2 = guess_with_key xs key2