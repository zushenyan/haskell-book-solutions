module Ch8.FixingDividedBy where

dividedBy :: Integral a => a -> a -> Maybe (a, a)
dividedBy _ 0 = Nothing
dividedBy num denom = Just $ go num denom 0
  where
    go n d count
      | n' < d' = (count, n')
      | otherwise = go (n' - d') d' (count + 1)
      where
        n' = abs n
        d' = abs d
