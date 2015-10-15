quasiQuicksort :: (Ord a) => [a] -> [a]
quasiQuicksort [] = []
quasiQuicksort (head:tail) = sortedSmaller ++ [pivot] ++ sortedLargerOrEqual
                           where pivot = head
                                 sortedSmaller = quasiQuicksort $ filter (< pivot) tail
                                 sortedLargerOrEqual = quasiQuicksort $ filter (>= pivot) tail