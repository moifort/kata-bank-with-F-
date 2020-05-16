module Feature

open Bank
open FsUnit
open NUnit.Framework
open Bank.Command


[<SetUp>]
let Setup () = ()

[<Test>]
let ``should print statement`` () =

    let printedStatement =
        createAccount 500.0
        |> makeDeposit 500.0
        |> makeWithdraw 100.0
        |> makeDeposit 1000.0
        |> print
        
    Assert.That
        (printedStatement,
         Is.EquivalentTo
             ([ "DATE       | AMOUNT  | BALANCE"
                "01/04/2014 | 500.00 | 1000.00"
                "01/04/2014 | -100.00 | 900.00"
                "01/04/2014 | 1000.00 | 1900.00" ]))
