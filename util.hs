
module Util where

insertAt :: a -> Int -> [a] -> [a]
insertAt x idx xs = start ++ [x] ++ end
                    where (start, end) = splitAt (max idx 0) xs

insertBefore :: a -> Int -> [a] -> [a]
insertBefore x idx = insertAt x (idx-1)

beforeAndAfter :: [a] -> Int -> ([a], a, [a])
beforeAndAfter items n
  | length items >= n = (init start, last start, end)
                        where (start, end) = splitBefore n items

splitBefore :: Int -> [a] -> ([a],[a])
splitBefore 0 arr = ([],arr)
splitBefore n arr = splitAt n arr

