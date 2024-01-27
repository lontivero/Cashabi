namespace Nostra.Client

open Nostra
open Nostra.Client.Request
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

    let filterDirectMessagesToMinter =
        Filter.all
        |> Filter.encryptedMessages
        //|> Filter.since DateTime.UtcNow
        |> fun f -> { f with PubKeys = [minterPubKey] }

    let announceParameters secret =
        issuerKey.ComputeCredentialIssuerParameters()
        |> CredentialIssuerParameters.serialize
        |> Event.createNote
        |> Event.sign secret

    let eventHandler publishToRelay secret subscriptionId event =
        if subscriptionId = DIRECT_MESSAGE_SUBSCRIPTION then
            event
            |> Event.decryptDirectMessage secret
            |> CredentialRequest.deserialize
            |> function
            | Ok credentialsRequest ->
                log "Credential request received"
                credentialsRequest
                |> issuer.HandleRequest
                |> CredentialResponse.serialize
                |> sayTo publishToRelay secret event.PubKey
                log $"New issued credentials sent back to requester ${credentialsRequest.Delta}"
            | Error _ ->
                log "Announcement was invalid"
        else
            log "The relay is crazy or what!?"

    let dispatchProtocolHandler publishToRelay secret =
        commonProtocolHandler (eventHandler publishToRelay secret)

    let start uri secret = async {
        let! relay = connectToRelay uri
        relay.subscribe DIRECT_MESSAGE_SUBSCRIPTION [filterDirectMessagesToMinter]

        relay.publish (announceParameters secret)
        let handleProtocolMessages = dispatchProtocolHandler relay.publish secret
        let protocolHandlingLoop = relay.startListening handleProtocolMessages

        do! protocolHandlingLoop
    }

