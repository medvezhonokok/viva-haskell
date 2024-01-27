module HW2.T2
  ( joinWith, 
    splitOn, 
  ) 
where 
 
import Data.List.NonEmpty (NonEmpty (..))
 
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator s = let (x:xs) = split_list separator s in x :| xs
  where
    split_list _ [] = [[]]
    split_list c' s' =
      let l = takeWhile (/= c') s'
          r = dropWhile (/= c') s'
      in l : if null r then [] else split_list c' (tail r) 
 
joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep (x :| xs) = concat (x : map (sep :) xs)