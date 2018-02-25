day_of_week :: Integer -> Integer -> Integer -> Integer
day_of_week y m d = (d + t1 + k + t2 + t3 + 5 * j) `mod` 7 where
  m' = if m <= 2  then m + 12 else m
  y' = if m <= 2  then y - 1 else y
  j = floor ((fromIntegral y') / 100.0)
  k = y' `mod` 100
  t1 = floor (fromIntegral (13 * (m' + 1)) / 5.0)
  t2 = floor (fromIntegral (k) / 4.0)
  t3 = floor (fromIntegral (j) / 4.0)
