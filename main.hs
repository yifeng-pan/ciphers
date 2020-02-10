import Data.Char


-- Trying to crack this
-- Key is 6 digits
code :: String
code = "UNFDNKEPBXPXNMFIOWHMIDNHHETEJVUNYIVOEXUFOCWVMDZRTBRETEVXYENEGPFOVQTLFRCVPBVUNQGHYMQFEKUIOVPKYUVFTXOEVXMNAJMTCWOEZRWBXLQVUNFATOYNOLVXNQQDZRXBAQVFGNJIPCZHFUNFGHQHFXGFASUVPHODZRYBFFMHCEOOKWEFIIZWBLAJTRENBHSRCXDZJFCJBVUDVGOIUZQBBMURNCGONEUNFXVXWXEFZIEJYIUSVRQKOWIQRIBYNGNDZRDGOLVQPGVXVWEWBMUREQUSVNWBYMVCVLGMOVVCMFERNDJVMTCVAEOAVCULNWZVFFJFCJOLQVPHMJQTHXEOVCDKQFOCXGPXWQVWOLVKCKTMOPUGXVZXZCFDNYRXAOMTUGKECFOGQTNFEQQKMWEHOKJORLJVBQVGUDCFMVVHCFYDAZOPGKYFUCWWASUDVBERZOUKIIQCGCAVQFZNFMJZHFJVMTCHTDPSEHNIUCNRQQVFZGYVCHPWBVWRWFIIJHPKBOUECXNZMVCBJVXVPHODZRYBJJWOENBHLIUBFHJWEXFDBZNGOLVQUGEIQVPHQHJXSKELCHDHJHVFGQUDVQNGQVVVPQNXVDRPBXBXTJOESRNRULCFYCAZMTUGBCVFYKIMBZNGOLVQGJFFMTWXELCHGQUDVQGJJXZWEGYDZXUXJKZVWBJFOFCJJXLXUAIIUVPKGMJHUGGLVQNHWIOHXHRQMWZGYVVQCEAOMOEMOEZRNLBDZRNGQFPDUNFAUHCKQFOCPKBOUVPKQIGVDGYVCFYNFOIWDEBVKHCFAZMYVBNMBVJGAHQHUNFJCQGFYFKHVLOLVQTCJVXHHXXIJRGVBVMOEMOLJWHMIDZRSKBQVCNGAJBXEJRONWAKEFPDCDUEWVPKXIVUTXHDNKEIPUCCDGYVCFYHMMXTUBBMOZCMBVMWRHMSQHUNFAFRNKNFYOEMOLVONPBAOWNFIDZQVRHLQDWXEEWPNKFVWOCVEXQFZVJDMDCYJSPXYKUFBSCFOLVSPHEXVREXAXCPECAJWOYNOMOPXROFPDUNFEMTCBOOQJCVFOBXBGQKMTCBVDMRZBAFUHCKNIUVVIFKNOEMJVMTCGOLVQZQYIIVTGQFOCUNFRNECXJVMTCYJSPXYKUDZRQEBXBRZMBPVCWFOLVEJQOLFXNDFAVHWXEQVUUFIICQMNJSUQCXRSNHCLVOMTCBQEJVPFIIWOCVEXXXIKXFKVVBASPOEMIMPDGHQHMTTALKJWIKUEWWBJGEJRGFOLVQVHEHFOEJMIUVVHOOQNAHQHEOBVBKVHXKRFTRBKUXIWDWAV"

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

-- Adds spaces to a message according to the key length
format_read :: String -> Int -> String
format_read [] _ = []
format_read xs n = take n xs' ++ " " ++ format_read (drop n xs') n
    where 
        xs' = xs

-- Formats a message for for a faster attack
format_attack :: String -> [Int]
format_attack xs = map chr2int (format xs)

attack2read :: [Int] -> Int -> String
attack2read xs = format_read xs'
    where
        xs' = map int2chr xs


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
percent n m = (fromIntegral n / fromIntegral m)


shift :: Char -> Int -> Char
shift c n
    | isUpper c = int2chr((chr2int c + n) `mod` 26)
    | otherwise = c

-- Second part of shift used for the Mixed Vigenère
-- shift2 :: Char -> [Char] -> Char
-- shift2 c alphabet
--     | isUpper c =int2chr (position alphabet c)
--     | otherwise = c

remove_dup :: String -> String
remove_dup xs = [x | (x, i) <- zip xs [0..(length xs - 1)], positions (take i xs) x == []]


-- Vigenere, Normal Operations

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


-- For Analysis by hand
-- TODO

-- Breakdown Vigenere into Caesar using a key length and an offset
caesar :: String -> Int -> Int -> String
caesar xs size offset = [x | (x, i) <- zip xs [0..(length xs - 1)], (i - offset) `mod` size == 0]

freqs :: String -> [(Char, Float)]
freqs xs = [(x, percent (count xs x) (length xs)) | x <- ['A'..'Z']]

qsort :: [(Char, Float)] -> [(Char, Float)]
qsort []     = []
qsort (p:xs) = (qsort greater) ++ [p] ++ (qsort lesser)
    where
        lesser  = [x | x <- xs, snd x < snd p]
        greater = [x | x <- xs, snd x >= snd p]

-- Frequency Analysis check to get a lookup table
check :: String -> Int -> Int -> [(Char, Float)]
check xs size offset = take 5 (qsort (freqs (caesar xs size offset)))

guess_alphabet :: [Char]
-- guess_alphabet = "V_____O____R___MB____F____"
guess_alphabet = "V__________R___M__________"

-- guess_offset :: String
-- guess_offset = "__Y_ET"

guess_alphabets :: [String]
guess_alphabets = [
        "____C_____________________",
        "__________________________",
        "____O_____________________",
        "__________________________",
        "____V_____________________",
        "__________________________"
    ]

-- TEMP
-- [(’C’,13.513513),(’E’,10.810811),(’U’,9.009008),(’N’,8.558558),(’V’,7.207207)]
-- [(’G’,11.711712),(’K’,11.261261),(’H’,9.009008),(’B’,8.558558),(’X’,8.108108)]
-- [(’O’,12.162163),(’F’,10.810811),(’B’,9.459459),(’A’,8.558558),(’J’,8.108108)]
-- [(’I’,9.90991),(’V’,9.459459),(’L’,9.459459),(’D’,9.459459),(’F’,9.009008)]
-- [(’V’,16.742083),(’M’,9.954751),(’Z’,8.144796),(’U’,7.692308),(’O’,7.2398195)]
-- [(’R’,10.859729),(’V’,9.954751),(’F’,8.597285),(’W’,8.144796),(’Q’,8.144796)]
        -- "____C______________E______",
        -- "__________________________",
        -- "____O______________F______",
        -- "__________________________",
        -- "____V__________R___M______",
        -- "____R___M__________V______"

guess :: String -> String
guess xs = [if (positions (alphabets!!(i `mod` l')) x) == [] then '_' else int2chr(position (alphabets!!(i `mod` l')) x) | (x,i) <- zip xs [0..l]]
    where
        alphabets = guess_alphabets
        l = length xs - 1
        l' = length alphabets

-- BAD
guess_with_offset :: String -> String -> String
guess_with_offset xs offset = [if (positions guess_alphabet x) == [] then '_' else int2chr(((position guess_alphabet x) + chr2int(offset!!(i `mod` l'))) `mod` 26) | (x,i) <- zip xs [0..l]]
    where
        l = length xs - 1
        l' = length offset

compare_guess :: String -> String -> String -> String
compare_guess xs offset1 offset2 = [if a == b then b else '_' | (a, b) <- zip guess1 guess2]
    where 
        guess1 = guess_with_offset xs offset1
        guess2 = guess_with_offset xs offset2

guess' :: String -> Int -> String
guess' xs n = format_read (guess xs) n

guess_with_offset' :: String -> String -> Int -> String
guess_with_offset' xs offset n = format_read (guess_with_offset xs offset) n

compare_guess' :: String -> String -> String -> Int -> String
compare_guess' xs offset1 offset2 n = format_read (compare_guess xs offset1 offset2) n 

add_guess :: String -> String -> String
add_guess xs ys = [if x == '_' then y else x | (x,y) <- zip xs ys]