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

    let create value: Withdraw =
        { Amount = Amount.create value
          Date = Date.create "01/04/2014" }

module Deposit =
    open Date
    open Amount

    type Deposit =
        { Date: Date
          Amount: Amount }

    let create value: Deposit =
        { Amount = Amount.create value
          Date = Date.create "01/04/2014" }

module Operation =
    open Withdraw
    open Deposit

    type Operation =
        | Withdraw of Withdraw
        | Deposit of Deposit

module Statement =
    open Operation
    open Amount

    type Statement =
        { Operation: Operation
          Balance: Amount }

    let balance { Balance = balance } = balance

    let create operation balance =
        { Operation = operation
          Balance = balance }

module Account =
    open Operation
    open Statement

    type Account =
        { Statements: Statement list }

    let create = { Statements = List.empty }

    let currentBalance statements =
        match statements with
        | [] -> Amount.create 0.0
        | [ statement ] -> statement.Balance
        | _ :: tail -> (List.last tail).Balance

    let updateBalance operation balance =
        match operation with
        | Deposit ({ Amount = amount }) -> Amount.add balance amount
        | Withdraw ({ Amount = amount }) -> Amount.subtract balance amount

    let addOperation operation account =
        let currentBalance = currentBalance account.Statements
        let newBalance = updateBalance operation currentBalance
        let newStatement = Statement.create operation newBalance
        { Statements = account.Statements @ [ newStatement ] }

module Print =
    open Date
    open Amount
    open Account
    open Statement
    open Operation

    let rec print (data: obj) =
        match data with
        | :? Amount as amount  -> Amount.get amount |> sprintf "%.2f"
        | :? Date as date -> Date.get date |> sprintf "%s"
        | _ -> "Nothing to print"

    let printStatement { Operation = operation; Balance = balance } =
        match operation with
        | Deposit { Date = date; Amount = amount } ->
            sprintf "%s | %s | %s" (print date) (print amount) (print balance)
        | Withdraw { Amount = amount; Date = date } ->
            sprintf "%s | -%s | %s" (print date) (print amount) (print balance)

    let printStatements account =
        "DATE       | AMOUNT  | BALANCE" :: (account.Statements
                                             |> List.skip 1
                                             |> List.map printStatement)

module Command =
    open Operation

    let makeWithdraw withdraw account =
        let withdraw = (Withdraw.create withdraw)
        Account.addOperation (Withdraw withdraw) account

    let makeDeposit deposit account =
        let deposit = Deposit.create deposit
        Account.addOperation (Deposit deposit) account

    let createAccount initialDeposit = Account.create |> makeDeposit initialDeposit

    let print account = Print.printStatements account
