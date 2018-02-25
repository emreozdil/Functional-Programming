-- Question 1

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = (d + t1 + k + t2 + t3 + 5 * j) `mod` 7 where
  m' = if m <= 2  then m + 12 else m
  y' = if m <= 2  then y - 1 else y
  j = floor ((fromIntegral y') / 100.0)
  k = y' `mod` 100
  t1 = floor (fromIntegral (13 * (m' + 1)) / 5.0)
  t2 = floor (fromIntegral (k) / 4.0)
  t3 = floor (fromIntegral (j) / 4.0)


-- Question 2

sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
  where
    sundays' :: Integer -> Integer -> Integer
    sundays' y m
      | y > end = 0
      | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
      where
        nextY = if m == 12 then y + 1 else y
        nextM = if m == 12 then 1 else m + 1
        rest = sundays' nextY nextM


-- Question 3

tailSundays1 :: Integer -> Integer -> Integer
tailSundays1 start end = sundays' start 1 0
  where
    sundays' :: Integer -> Integer -> Integer -> Integer
    sundays' y m rest
      | y > end = rest
      | m == 13 = sundays' (y + 1) (1) rest
      | otherwise = if dayOfWeek y m 1 == 1 then sundays' y (m + 1) (rest + 1) else sundays' y (m + 1) rest


-- Question 4

leap :: Integer -> Bool
leap y = y `mod` 4 == 0 && y `mod` 100 /= 0 || y `mod` 400 == 0


dayInMonth :: Integer -> Integer -> Integer
dayInMonth m y
  | m == 2  && leap y = 29
  | m == 2 = 28
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise = 31


sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays' start 1 2
  where
    sundays' :: Integer -> Integer -> Integer -> Integer
    sundays' y m weekday
      | y > end = 0
      | otherwise = if nextWeekday `mod` 7 == 0 then rest + 1 else rest
      where
        nextWeekday = weekday + (dayInMonth m y) `mod` 7
        nextY = if m == 12 then y + 1 else y
        nextM = if m == 12 then 1 else m + 1
        rest = sundays' nextY nextM nextWeekday


-- Question 5

-- In any 400 years there is always (400 * 365) + (400/4) - (400/100) + (400/400) days.
-- 146097 days in 400 years is fully divided into 7.
-- The possibility that a certain day of a month is a Sunday is 1/7.
-- Yes, all days equally possible.
