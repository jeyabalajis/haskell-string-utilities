isVowel :: Char -> Bool
isVowel x = x `elem` ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']

isNotVowel :: Char -> Bool
isNotVowel x = not (isVowel x)

recursiveAlternateVowels :: String -> Bool -> Bool
recursiveAlternateVowels xs v = go xs v where
    go [] _ = True
    go (x:xs) True = isVowel x && go xs False
    go (x:xs) False = isNotVowel x && go xs True

{-|
  This function takes a string and validates
  whether it is composed of alternate vowels
-}
wordContainsAlternateVowels :: String -> Bool
wordContainsAlternateVowels [] = True
wordContainsAlternateVowels (x:xs)
    | isVowel x = recursiveAlternateVowels ([x]++xs) True
    | isNotVowel x = recursiveAlternateVowels ([x]++xs) False