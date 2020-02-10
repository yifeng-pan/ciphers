# Mixed Vigenère Cipher
A description of the cipher can be found in "Cryptanalysis Using Nature-Inspired Optimization Algorithms" by Karel P. Bergmann, Chapter 6.

## Cryptanalysis
Load "analysis.hs" for tools for cryptanalysis by hand.

Example:
```
*Main> message = example_text
*Main> key = "KEY"
*Main> code = encode message key

*Main> check code 3 0
[('N',12.921349),('A',12.35955),('Q',10.674157),('X',7.865169),('W',7.865169)]
*Main> check code 3 1
[('G',12.4293785),('B',11.299435),('R',8.474577),('W',7.909604),('U',7.3446326)]
*Main> check code 3 2
[('Y',12.4293785),('Q',9.039548),('X',7.3446326),('L',7.3446326),('E',7.3446326)]

*Then solve using the guess functions*
```
"check" can also be used to guess the key length aswell.

## Brute-force
Load "attack.hs" for a brute-force attack using frequency analysis.
+ Only feasible when the key is very short.
+ Decreasing the message length increases speed, but also increases false positives.
+ The "attack" function returns the key with the lowest test-statistic according to the chi-squared test.

Example:
```
*Main> message = format_attack example_text
*Main> key = format_attack "KEY"
*Main> code = encode message key

*Main> attack code 3
([10,4,24],3.4656312)
*Main> attack (take 50 code) 3
([10,4,24],33.51339)
*Main> attack (take 15 code) 3
([10,4,24],66.54879)
*Main> attack (take 10 code) 3
([10,2,20],92.09451)

*Main> original_message = format_read (decode code [10,4,24])
```