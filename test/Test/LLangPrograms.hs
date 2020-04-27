{-# LANGUAGE QuasiQuotes #-}

module Test.LLangPrograms where

import Test.Tasty.HUnit
import Text.RawString.QQ

import LLang
import AST
import Combinators

isFailure (Failure _) = True
isFailure  _          = False
assertNotFailure r = assertBool "" $ not $ isFailure r

textFactorial = tail [r|
read (n);
result = 1;
while (n > 1) {
    result = result * n;
    n = n - 1;
}
write (result);
|]
stmtFactorial =
  Seq
    [ Read "n"
    , Assign "result" (Num 1)
    , While (BinOp Gt (Ident "n") (Num 1))
            (Seq
              [ Assign "result" (BinOp Mult (Ident "result") (Ident "n"))
              , Assign "n" (BinOp Minus (Ident "n") (Num 1))
              ]
            )
    , Write (Ident "result")
    ]
unit_factorial :: Assertion
unit_factorial = runParser parseL textFactorial @?= Success (InputStream "" $ Position 8 0) Nothing stmtFactorial

textFibonacci = tail [r|
read (n);
prev = 0;
curr = 1;
while (n > 0) {
    next = prev + curr;
    prev = curr;
    curr = next;
    n = n - 1;
}
write (prev);
|]
stmtFibonacci =
  Seq
    [ Read "n"
    , Assign "prev" (Num 0)
    , Assign "curr" (Num 1)
    , While (BinOp Gt (Ident "n") (Num 0))
            (Seq
              [ Assign "next" (BinOp Plus (Ident "prev") (Ident "curr"))
              , Assign "prev" (Ident "curr")
              , Assign "curr" (Ident "next")
              , Assign "n" (BinOp Minus (Ident "n") (Num 1))
              ]
            )
    , Write (Ident "prev")
    ]
unit_fibonacci :: Assertion
unit_fibonacci = runParser parseL textFibonacci @?= Success (InputStream "" $ Position 11 0) Nothing stmtFibonacci


fromResultProgram pr = let ~(Success _ _ result) = runParser parseProg pr in result

textFibonacciRecursive = tail [r|
def fib(n) {
    if (n <= 1) then {
        return (n);
    } else {
        return (fib(n - 1) + fib(n - 2));
    }
}

def main() {
    read (n);
    write (fib(n));
}
|]
programFibonacciRecursive =
  Program {
      functions = [
        let
          fib1 = FunctionCall "fib" [BinOp Minus (Ident "n") (Num 1)]
          fib2 = FunctionCall "fib" [BinOp Minus (Ident "n") (Num 2)]
         in Function "fib" ["n"] (Seq [If (BinOp Le (Ident "n") (Num 1)) (Seq [Return $ Ident "n"]) (Seq [Return $ BinOp Plus fib1 fib2])])
      ],
      main = Seq [Read "n", Write (FunctionCall "fib" [Ident "n"])]
  }
unit_fibonacci_recursive :: Assertion
unit_fibonacci_recursive = fromResultProgram textFibonacciRecursive @?= programFibonacciRecursive

textFactorialRecursive = tail [r|
def factorial(n) {
    if (n == 0) then {
        return (1);
    } else {
        return (n * factorial(n - 1));
    }
}

def main() {
    read (n);
    write (factorial(n));
}
|]
programFactorialRecursive =
  Program {
      functions = [
        let prev = FunctionCall "factorial" [BinOp Minus (Ident "n") (Num 1)]
         in Function "factorial" ["n"] (Seq [If (BinOp Equal (Ident "n") (Num 0)) (Seq [Return $ Num 1]) (Seq [Return $ BinOp Mult (Ident "n") prev])])
      ],
      main = Seq [Read "n", Write (FunctionCall "factorial" [Ident "n"])]
  }
unit_factorial_recursive :: Assertion
unit_factorial_recursive = fromResultProgram textFactorialRecursive @?= programFactorialRecursive
