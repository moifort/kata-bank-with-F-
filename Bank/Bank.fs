namespace Bank

module Date =
    type Date = Date of string

    let get (Date date) = date
    let create value = Date value

module Amount =
    type Amount = private | Amount of double

    let get (Amount amount) = amount
    let create value = Amount value
    let add (Amount amount1) (Amount amount2) = Amount(amount1 + amount2)
    let subtract (Amount amount1) (Amount amount2) = Amount(amount1 - amount2)
    let negate (Amount amount) = create -amount

module Withdraw =
    open Date
    open Amount

    type Withdraw =
        { Date: Date
          Amount: Amount }

    let create value date: Withdraw =
        { Amount = Amount.create value
          Date = date }

module Deposit =
    open Date
    open Amount

    type Deposit =
        { Date: Date
          Amount: Amount }

    let create value date: Deposit =
        { Amount = Amount.create value
          Date = date }

module Action =
    open Withdraw
    open Deposit

    type Action =
        | Withdraw of Withdraw
        | Deposit of Deposit

module Balance =
    open Amount
    open Action
    open Deposit
    open Withdraw

    type Balance = Balance of Amount

    let get (Balance balance) = balance

    let rec create (value: obj) =
        match value with
        | :? double as doubleValue ->
            doubleValue
            |> Amount.create
            |> create
        | :? Amount as amountValue -> amountValue |> Balance
        | _ -> failwithf "Cannot create Balance from %A" value

    let calculate action balance =
        let balanceAmount = get balance
        match action with
        | Deposit { Amount = amountToAdd } -> Amount.add balanceAmount amountToAdd |> create
        | Withdraw { Amount = amountToSubtract } -> Amount.subtract balanceAmount amountToSubtract |> create

module Transaction =
    open Action
    open Balance

    type Transaction =
        { Action: Action
          Balance: Balance }

    let balance { Balance = balance } = balance

    let create action balance =
        { Action = action
          Balance = balance }

module Account =
    open Transaction

    type Account =
        { Transactions: Transaction list }

    let create = { Transactions = List.empty }

    let currentBalance { Transactions = transactions } =
        match transactions with
        | [] -> Balance.create 0.0
        | [ transaction ] -> transaction.Balance
        | _ :: tail -> (List.last tail).Balance

    let addTransaction action account =
        let currentBalance = currentBalance account
        let newBalance = Balance.calculate action currentBalance

        let newTransaction = Transaction.create action newBalance
        { Transactions = account.Transactions @ [ newTransaction ] }

module Print =
    open Date
    open Account
    open Action
    open Amount
    open Balance
    open Transaction
    
    let rec print (data: obj) =
        match data with
        | :? Amount as amount -> Amount.get amount |> sprintf "%.2f"
        | :? Balance as balance -> Balance.get balance |> print
        | :? Date as date -> Date.get date |> sprintf "%s"
        | _ -> "Nothing to print"

    let printTransaction { Action = action; Balance = balance } =
        match action with
        | Deposit { Date = date; Amount = amount } ->
            sprintf "%s | %s | %s" (print date) (print amount) (print balance)
        | Withdraw { Amount = amount; Date = date } ->
            sprintf "%s | -%s | %s" (print date) (print amount) (print balance)
    
    let printStatement transactions = "DATE       | AMOUNT  | BALANCE" :: (transactions
                                             |> List.skip 1
                                             |> List.map printTransaction) 
module Command =
    open Action
    open Account
    
    let createAccount initialDeposit =
        Account.addTransaction (Deposit initialDeposit) Account.create

    let makeWithdraw withdraw account =
        Account.addTransaction (Withdraw withdraw) account

    let makeDeposit deposit account =
        Account.addTransaction (Deposit deposit) account

    let printStatement account = Print.printStatement account.Transactions
       


