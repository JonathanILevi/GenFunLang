module Gen.FunArray where

funOp :: (Bool->Bool->Bool) -> [(a->Bool)] -> a -> Bool
funOp com (f:f2:[]) x = (f x) `com` (f2 x)
funOp com (f:fs) x = (f x) `com` (funOp com fs x)
funOr = funOp (||)
funAnd = funOp (||)
