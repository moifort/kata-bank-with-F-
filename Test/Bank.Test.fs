module Feature

open Bank
open FsUnit
open NUnit.Framework
open Bank.Command


[<SetUp>]
let Setup () = ()

[<Test>]
let ``should print statement`` () =
    let date = Date.create "01/04/2014"
    let account = createAccount (Deposit.create 500.0 date)
    let creditedAccount = makeDeposit (Deposit.create 500.0 date) account
    let debitedAccount = makeWithdraw (Withdraw.create 100.0 date) creditedAccount
    let recreditedAccount = makeDeposit (Deposit.create 1000.0 date) debitedAccount
    let printedStatement = printStatement recreditedAccount
    Assert.That
        (printedStatement,
         Is.EquivalentTo
             ([ "DATE       | AMOUNT  | BALANCE"
                "01/04/2014 | 500.00 | 1000.00"
                "01/04/2014 | -100.00 | 900.00"
                "01/04/2014 | 1000.00 | 1900.00" ]))
