open System
open Microsoft.FSharp.Control
open Nostra.Client
open Nostra
open Nostra.Client.ECashClient

let startWallet uri =
    let secret = SecretKey.createNewRandom ()
    let pubkey = SecretKey.getPubKey secret
    Console.WriteLine $"My public Key: {Shareable.encodeNpub pubkey}"

    async {
        let! relay = connectToRelay uri
        let protocolHandlerPush = protocolHandlerLoop secret relay.publish
        let handleProtocolMessages = dispatchProtocolHandler secret protocolHandlerPush
        let readCommandFromUserLoop = processUserCommandLoop protocolHandlerPush
        let protocolHandlingLoop = relay.startListening handleProtocolMessages
        subscribeToAnnouncements relay.subscribe
        subscribeToDirectMessages relay.subscribe pubkey

        do! (Async.Parallel [ protocolHandlingLoop; readCommandFromUserLoop ] |> Async.Ignore)
    } |> Async.RunSynchronously

let startMinter uri =
    let secret = Common.secretFromHex "1adafb0e2e40d3397688f8351acb92d09d5edfe532226b6920a4904123abfad9"
    async {
        let! relay = connectToRelay uri
        ECashMinter.subscribeToDirectMessagesToMinter relay.subscribe
        ECashMinter.announceParameters relay.publish secret

        let handleProtocolMessages = ECashMinter.dispatchProtocolHandler relay.publish secret
        let protocolHandlingLoop = relay.startListening handleProtocolMessages

        do! protocolHandlingLoop
    } |> Async.RunSynchronously

[<EntryPoint>]
let main argv =
    //let uri = Uri("wss://nostr-pub.wellorder.net")
    let uri = Uri("ws://127.0.0.1:8080/")

    match argv[0] with
    | "wallet" -> startWallet uri
    | "minter" -> startMinter uri
    | _ ->
        Console.WriteLine "Only 'wallet' and 'minter' are valid arguments."
    0