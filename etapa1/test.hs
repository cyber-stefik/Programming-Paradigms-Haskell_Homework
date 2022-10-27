import qualified Data.Set as S

myFunc :: Ord a =>[a] -> [a]
myFunc list = 
    foldl (\acc x -> case () of
                | even x = True -> [x] ++ acc
                | otherwise -> x : acc) [] list