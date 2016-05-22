module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend s name val = \x -> if x == name then val else s x

empty :: State
empty = \_ -> 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE s (Var v) = s v
evalE s (Val x) = x
evalE s (Op e1 Plus e2) = (evalE s e1) + (evalE s e2)
evalE s (Op e1 Minus e2) = (evalE s e1) - (evalE s e2)
evalE s (Op e1 Times e2) = (evalE s e1) * (evalE s e2)
evalE s (Op e1 Divide e2) = (evalE s e1) `div` (evalE s e2)
evalE s exp = 
  let convertBool x = if x then 1 else 0 in
    case exp of
      Op e1 Gt e2 -> convertBool $ (evalE s e1) > (evalE s e2)
      Op e1 Ge e2 -> convertBool $ (evalE s e1) >= (evalE s e2)
      Op e1 Lt e2 -> convertBool $ (evalE s e1) < (evalE s e2)
      Op e1 Le e2 -> convertBool $ (evalE s e1) <= (evalE s e2)
      Op e1 Eql e2 -> convertBool $ (evalE s e1) == (evalE s e2)

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign v exp) = DAssign v exp
desugar (Incr v) = DAssign v (Op (Var v) Plus (Val 1))
desugar (If exp trueStmt falseStmt) = DIf exp (desugar trueStmt) (desugar falseStmt)
desugar (While exp loopStmt) = DWhile exp (desugar loopStmt)
desugar (For initStmt condExp updateStmt loopStmt) = 
  DSequence (desugar initStmt) (DWhile condExp (DSequence (desugar loopStmt) (desugar updateStmt)))
desugar (Sequence stmt1 stmt2) = DSequence (desugar stmt1) (desugar stmt2)
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign v exp) = extend s v (evalE s exp)
evalSimple s (DIf exp trueDStmt falseDStmt) = if (evalE s exp) /= 0 then evalSimple s trueDStmt else evalSimple s falseDStmt
evalSimple s (DWhile exp loopStmt) = if (evalE s exp) /= 0 then evalSimple (evalSimple s loopStmt) (DWhile exp loopStmt) else s
evalSimple s (DSequence dStmt1 dStmt2) = evalSimple (evalSimple s dStmt1) dStmt2
evalSimple s DSkip = s

run :: State -> Statement -> State
run s stmt = evalSimple s (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
