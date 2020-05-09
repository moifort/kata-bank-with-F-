namespace Bank.Type

open System

type User = private | User of string

type Money = private | Money of double

module User =
    let value (User user) = user

    let create name =
        match name with
        | null -> Error "No name found."
        | "" -> Error "Name is empty."
        | _ -> Ok(User name)
