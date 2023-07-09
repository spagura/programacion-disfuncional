module Dobble.Utils where

import System.Random (randomRIO)

-- Funcion para hacerle shuffle a una lista
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle items = do
    index <- randomRIO (0, length items - 1)
    rest <- shuffle (take index items ++ drop (index + 1) items)
    return (items !! index : rest)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

printToTextFile :: String -> FilePath -> IO ()
printToTextFile content filePath = do
  writeFile filePath content