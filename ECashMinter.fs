namespace Nostra.Client

open System
open Nostra
open Nostra.Client.Request
open Thoth.Json.Net
open WabiSabi.Crypto
open WabiSabi.Crypto.Randomness

module ECashMinter =
    open Nostra.Client
    open Nostra.Client.Common
    open WabiSabi

    [<Literal>]
    let DIRECT_MESSAGE_SUBSCRIPTION = "direct messages to the minter"

    let maxAmount = (2L <<< 32) - 1L
    let rnd = SecureRandom()
    let issuerKey = CredentialIssuerSecretKey(rnd)
    let issuer = CredentialIssuer(issuerKey, rnd, maxAmount)

    let subscribeToDirectMessagesToMinter subscribeTo =
        let filter =
            Filter.all
            |> Filter.encryptedMessages
            //|> Filter.since DateTime.UtcNow
            |> fun f -> { f with PubKeys = [minterPubKey] }
        subscribeTo DIRECT_MESSAGE_SUBSCRIPTION [filter]

    let announceParameters send secret =
        let parameters = issuerKey.ComputeCredentialIssuerParameters()
        let announcementEvent =
            parameters
            |> Encode.issuerParameters
            |> Encode.toString 0
            |> Event.createNote
            |> Event.sign secret

        send announcementEvent

    let dispatchProtocolHandler relay secret =
        commonProtocolHandler
            (fun subscriptionId event ->
                match subscriptionId with
                | DIRECT_MESSAGE_SUBSCRIPTION ->
                    let issuanceRequestResult =
                        event
                        |> Event.decryptDirectMessage secret
                        |> Decode.fromString Decode.credentialsRequest
                    match issuanceRequestResult with
                    | Ok credentialsRequest ->
                        Console.WriteLine "Credential request received"
                        let credentialsResponseJson =
                            issuer.HandleRequest credentialsRequest
                            |> Encode.credentialsResponse
                            |> Encode.toString 0
                        sayTo relay secret event.PubKey credentialsResponseJson
                        Console.WriteLine $"New issued credentials sent back to requester ${credentialsRequest.Delta}"
                    | Error e -> Console.WriteLine "Announcement was invalid"
                | _ -> Console.WriteLine "The relay is crazy or what!?")
