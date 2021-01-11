data FailableDouble = Failure
                    | OK Double
  deriving Show

ex01 = Failure
ex02 = OK 3.4

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

main = print (failureToZero ex01, failureToZero ex02)
