import Numeric

-- readHex :: String -> [(Int, String)]
-- head :: [(Int, String)] -> (Int, String)
-- fst :: (Int, String) -> Int

myReadHex :: String -> Integer
myReadHex = fst . head . readHex


-- intToB64Char :: Int -> Char
-- intToB64Char x
--   | x < 26                = ['A'..'Z'] !! (x - (0))
--   | x < 26 + 26           = ['a'..'z'] !! (x - (26))
--   | x < 26 + 26 + 10      = ['0'..'9'] !! (x - (26 - 26))
--   | x < 26 + 26 + 10 + 2  = ['+', '/'] !! (x - (26 - 26 - 10))
--   | x > 64                = error "number too big"


intToB64Char :: Int -> Char
intToB64Char x
  | x < 64     = d !! x
  | otherwise  = error "number too big"
  where
    d = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/']

integerToB64 :: Integer -> String
integerToB64 x
  | x < 64     = [intToB64Char $ fromIntegral x]
  | otherwise  = (integerToB64 $ x `quot` 64) ++ [intToB64Char $ fromIntegral x `mod` 64]

-- Tail recursive
integerToB64' :: Integer -> String
integerToB64' x = f x ""
  where
    f :: Integer -> String -> String
    f x str
      | x < 64     = [intToB64Char $ fromIntegral x]
      | otherwise  = integerToB64' (x `quot` 64) strNew
        where
          strNew = (str [intToB64Char $ fromIntegral x `mod` 64])

-- Fold
integerToB64'' :: Integer -> String


hexToB64 :: String -> String
hexToB64 = integerToB64 . myReadHex


main = putStrLn "hello world"
