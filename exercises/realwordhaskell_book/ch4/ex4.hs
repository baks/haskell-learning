import System.Environment (getArgs)

align :: String -> String -> (String, String)
align a b = case length a `compare` length b of GT -> (a, b ++ take (length a) (repeat ' '))
                                                LT -> (a ++ take (length b) (repeat ' '), b)
                                                EQ -> (a,b)
                                                
align' :: Int -> String -> String
align' n str = case length str `compare` n of GT -> str
                                              LT -> str ++ take (n-(length str)) (repeat ' ')
                                              EQ -> str

concatPair :: (Char, Char) -> String
concatPair pair = [fst pair, snd pair]

alignEveryElemInList list = [align' max newEl | newEl <- list]
    where max = maximum [length el | el <- list]

transposeTwo :: String -> String -> [String]
transposeTwo a b = [concatPair pair | pair <- zip x y]
     where (x,y) = align a b

transpose' :: [String] -> [String]
transpose' [] = []
transpose' [x] = [el : '\n' :[] | el <- x]
transpose' (x:y:[]) = transposeTwo x y
transpose' (x:y:xs) = zipWith (++) (transposeTwo x y) (transpose' xs)

transpose :: String -> String
transpose input = unlines (transpose' (alignEveryElemInList (words input)))

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = transpose