module Stack (Stack,
              pilhaVazia,
              pilhaInserir,
              pilhaPop,
              pilhaTopo,
              pilhaSoma,
              pilhaSubt,
              pilhaMult,
              pilhaDivi,
              pilhaDup,
              pilhaSwap,
              pilhaSize,
              pilhaDifer,
              pilhaMaior,
              pilhaMenor,
              pilhaIgual,
              pilhaAnd,
              pilhaOr,
              pilhaMod
              ) where

data Stack a = Stack [a]


pilhaVazia :: Stack a
pilhaVazia = (Stack [])

pilhaInserir :: Integer -> Stack Integer -> Stack Integer
pilhaInserir x (Stack xs) = Stack (ins x xs)
                        where ins x xs = (x:xs)
    
pilhaTopo :: Stack Integer -> Integer
pilhaTopo (Stack (x:xs)) = x
    
pilhaPop :: Stack Integer -> Stack Integer
pilhaPop (Stack x) = Stack (ins x)
                where ins (x:xs) = (xs)

pilhaSoma :: Stack Integer -> Stack Integer
pilhaSoma (Stack x) = Stack (ins x)
                where ins (x:xs:ys) = (x+xs):(ys)
                
pilhaSubt :: Stack Integer -> Stack Integer
pilhaSubt (Stack x) = Stack (ins x)
                where ins (x:xs:ys) = (xs-x):(ys)

pilhaMult :: Stack Integer -> Stack Integer
pilhaMult (Stack x) = Stack (ins x)
                where ins (x:xs:ys) = (x*xs):(ys)

pilhaDivi :: Stack Integer -> Stack Integer
pilhaDivi (Stack x) = Stack (ins x)
                where ins (x:xs:ys) = (div xs x):(ys)
                
pilhaDup :: Stack Integer -> Stack Integer
pilhaDup (Stack x) = Stack (ins x)
                where ins (x:ys) = (x:x:ys)
                
pilhaSwap :: Stack Integer -> Stack Integer
pilhaSwap (Stack x) = Stack (ins x)
                where ins (x:xs:ys) = (xs:x:ys)

pilhaSize :: Stack Integer -> Stack Integer
pilhaSize (Stack x) = Stack (ins x)
                where ins (x) = (toInteger (length (x))):x
                
pilhaDifer :: Stack Integer -> Stack Integer
pilhaDifer (Stack x) = Stack (ins x)
                where ins (x:xs)
                            | x == 0 = 1:xs
                            | otherwise = 0:xs
                            
pilhaMaior :: Stack Integer -> Stack Integer
pilhaMaior (Stack x) = Stack (ins x)
                where ins (x:xs:ys)
                            | x > xs = 0:ys
                            | otherwise = 1:ys


pilhaMenor :: Stack Integer -> Stack Integer
pilhaMenor (Stack x) = Stack (ins x)
                where ins (x:xs:ys)
                            | x < xs = 0:ys
                            | otherwise = 1:ys


pilhaIgual :: Stack Integer -> Stack Integer
pilhaIgual (Stack x) = Stack (ins x)
                where ins (x:xs:ys)
                            | x == xs = (1:ys)
                            | otherwise = (0:ys)

pilhaOr :: Stack Integer -> Stack Integer
pilhaOr (Stack x) = Stack (ins x)
                where ins (x:xs:ys)
                            | (x == 1 || xs == 1) = (1:ys)
                            | otherwise = (0:ys)

pilhaAnd :: Stack Integer -> Stack Integer
pilhaAnd (Stack x) = Stack (ins x)
                where ins (x:xs:ys)
                            | (x == 1 && xs == 1) = (1:ys)
                            | otherwise = (0:ys)
                            
pilhaMod :: Stack Integer -> Stack Integer
pilhaMod (Stack x) = Stack (ins x)
                where ins (x:xs:ys) = (mod xs x):(ys)

instance Show a => Show (Stack a) where
    show (Stack stack) = "<-> " ++ printStack stack ++ "]"
        where
             printStack []     = " "
             printStack [x]    = show x
             printStack (x:xs) = show x ++ "," ++ printStack xs