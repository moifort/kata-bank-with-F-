module TestBank

open Bank.Core
open Bank.Type
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``should create a new user call thibaut`` () =
    // When
    let createdUser = newUser
    
    // Then
    match newUser with
    | Ok user -> Assert.AreEqual(User.value user, "thibaut")
    | Error error -> Assert.Fail(error)
    