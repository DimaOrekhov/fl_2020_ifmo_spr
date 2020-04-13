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
read n;
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
unit_factorial = runParser parseL textFactorial @?= Success "" stmtFactorial

textFibonacci = tail [r|
read n;
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
unit_fibonacci = runParser parseL textFibonacci @?= Success "" stmtFibonacci
