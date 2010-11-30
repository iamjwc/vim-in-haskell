
module Util where

insertAt :: a -> Int -> [a] -> [a]
insertAt x idx xs = start ++ [x] ++ end
                    where (start, end) = splitAt (max idx 0) xs

insertBefore :: a -> Int -> [a] -> [a]
insertBefore x idx = insertAt x (idx-1)
