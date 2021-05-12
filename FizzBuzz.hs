-- FizzBuzz: http://wiki.c2.com/?FizzBuzzTest

fizzbuzz x
       | fizz x && buzz x  = "FizzBuzz"
       | fizz x            = "Fizz"
       | buzz x            = "Buzz"
       | otherwise         = ""
       where
          fizz n = (n `mod` 3) == 0
          buzz n = (n `mod` 5) == 0
       
main = putStrLn $ show $ map fizzbuzz [1..100]