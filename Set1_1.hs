import Numeric


hexInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
expectedB64Output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"


-- | myReadHex is just what readHex should be intuitively
myReadHex :: String -> Integer
myReadHex = fst . head . readHex
-- readHex :: String -> [(Int, String)]


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

-- Tail recursive (doesn't work yet??)
-- integerToB64' :: Integer -> String
-- integerToB64' x = f x ""
--   where
--     f :: Integer -> String -> String
--     f x str
--       | x < 64     = [intToB64Char $ fromIntegral x]
--       | otherwise  = f (x `quot` 64) strNew
--         where
--           strNew = str ++ [intToB64Char $ fromIntegral x `mod` 64]

-- Fold and iterate?
-- integerToB64'' :: Integer -> String


hexToB64 :: String -> String
hexToB64 = integerToB64' . myReadHex


-- TODO: try directly converting list of Word16's to list of Word64's
