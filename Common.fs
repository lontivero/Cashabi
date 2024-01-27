namespace Nostra.Client

open System
open Nostra
open Nostra.Client
open Thoth.Json.Net
open NBitcoin.Secp256k1
open WabiSabi.CredentialRequesting
open WabiSabi.Crypto
open WabiSabi.Crypto.ZeroKnowledge

module Common =

    type PrivateSender = ECPrivKey -> string -> unit

    type Money = int64
    type MinterMessage =
         | MintParametersAnnouncement of CredentialIssuerParameters
         | NewCredentialsMinted of CredentialsResponse

    type PayerMessage =
         | NewPaymentReceived of Credential

    type UserMessage =
         | ConsolidateCredential
         | SendTo of Money * AuthorId
         | Buy of Money
         | GetNullCredentials

    type EchashProtocolMessage =
         | MinterMessage of MinterMessage
         | UserMessage of UserMessage
         | PayerMessage of PayerMessage

    let secretFromHex =
         Utils.fromHex >> ECPrivKey.Create >> SecretKey

    let pubKeyFromHex =
        Decode.fromString Decode.authorId

    let sayTo send secret pubKey msg =
        Event.createEncryptedDirectMessage pubKey secret msg
        |> Event.sign secret
        |> send

    let minterPubKey =
        "'448d8a020ef05ffd37e2e2044b8e2f791b2b6ea38f5cbde4c58f09f992065c81'"
        |> pubKeyFromHex
        |> function
            | Ok pubkey -> pubkey
            | _ -> failwith "That's not valid xonlypubkey"

    let log (str: string) =
        Console.WriteLine str

    let commonProtocolHandler eventsHandler = function
        | Ok (Response.RMEvent (subscriptionId, event)) ->
            eventsHandler subscriptionId event
        | Ok (Response.RMACK(eventId, success, message)) ->
            let (EventId eid) = eventId
            log $"Event: {eid |> Utils.toHex} Success: {success} = {message}"
        | Ok (Response.RMNotice message) ->
            log message
        | Ok (Response.RMEOSE subscriptionId) ->
            log $">>> {subscriptionId} Done"
        | Error e ->
            log (e.ToString())

