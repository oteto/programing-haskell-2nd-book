-- p.127 - p.128

getLine' :: IO String
getLine' = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- getLine'
      return (x : xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = do
  putChar x
  putStr' xs

putStrln' :: String -> IO ()
putStrln' xs = do
  putStr' xs
  putChar '\n'

strlen :: IO ()
strlen = do
  putStr' "Enter a string: "
  xs <- getLine'
  putStr' "The string has "
  putStr' (show (length xs))
  putStrln' " charactors."