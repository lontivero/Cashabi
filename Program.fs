open System
open Microsoft.FSharp.Control
open Nostra.Client
open Nostra

[<EntryPoint>]
let main argv =
    let uri = Uri("ws://127.0.0.1:8080/")

    match argv[0] with
    | "wallet" ->
        let secret = SecretKey.createNewRandom ()
        let pubkey = SecretKey.getPubKey secret
        Console.WriteLine $"My public Key: {Shareable.encodeNpub pubkey}"
        ECashClient.start uri secret pubkey |> Async.RunSynchronously
    | "minter" ->
        let secret = Common.secretFromHex "1adafb0e2e40d3397688f8351acb92d09d5edfe532226b6920a4904123abfad9"
        ECashMinter.start uri secret |> Async.RunSynchronously
    | _ ->
        Console.WriteLine "Only 'wallet' and 'minter' are valid arguments."
    0