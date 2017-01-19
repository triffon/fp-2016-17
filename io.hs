main = do line <- getLine
          putStrLn $ "Въведохте: " ++ line

getInt :: IO Int
getInt = do line <- getLine
            return $ read line

findAverage :: IO Double
findAverage = do putStr "Моля, въведете брой: "
                 n <- getInt
                 s <- readAndSum n
                 return $ fromIntegral s / fromIntegral n


-- прочита n числа и връща сумата им
readAndSum :: Int -> IO Int
readAndSum 0 = return 0
readAndSum n = do putStr "Моля, въведете число: "
                  x <- getInt
                  s <- readAndSum (n-1)
                  return $ s + x

averageMain = do avg <- findAverage
                 putStrLn $ "Средното аритметично е: " ++ show avg

getInts n = sequence (replicate n getInt)

printRead s = do putStr (s ++ " = ")
                 getInt


printList l = mapM_ print l
