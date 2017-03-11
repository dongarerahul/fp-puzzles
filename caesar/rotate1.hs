import Data.Char

rotate :: Int -> String -> String
rotate 0 xs = xs
rotate n xs = rotate (n-1) (last xs : init xs)

rotate2 n xs = drop n xs ++ take n xs

-- Won't work in case rotation count is greater than length of array
rotate1 :: Int -> [a] -> [a]
rotate1 n xs
  | n < 0                   = xs
  | n > 0 && n > (length xs) = (x ++ y) where (y, x) = splitAt n xs
  -- | otherwise                = rotate1 (mod n $ length xs) xs

makeKey :: Int -> [(Char, Char)]
makeKey n = zip list $ rotate2 n list  where list = ['A'..'Z']

lookup' :: Char -> [(Char, Char)] -> Char
lookup' key [] = key
lookup' key (x:xs) = if (fst x) == key then snd x else lookup' key xs

--lookup' c ((k, v) : xs) = if k == c then v  else lookup c xs

encipher :: Int -> Char -> Char
encipher n c = lookup' c $ makeKey n

normalize :: String -> String
normalize [] = []
normalize (x:xs)
  | isAlphaNum x = toUpper x : normalize xs
  | otherwise = normalize xs

enc :: Int -> String -> String
enc _ [] = []
enc n (x:xs) = encipher n x : enc n xs

encipherStr :: Int -> String -> String
encipherStr _ [] = []
encipherStr n text = enc n $ normalize text
