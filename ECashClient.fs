namespace Nostra.Client

open System
open System.Threading
open Nostra.Client.Common
open Nostra
open Nostra.Client.Request
open Thoth.Json.Net
open WabiSabi.CredentialRequesting
open WabiSabi.Crypto
open WabiSabi.Crypto.Randomness
open WabiSabi.Crypto.ZeroKnowledge

module ECashClient =
    open Nostra.Client
    open WabiSabi

    [<Literal>]
    let MINT_ANNOUNCEMENTS_SUBSCRIPTION = "minter's announcements"
    [<Literal>]
    let DIRECT_MESSAGE_SUBSCRIPTION = "direct messages"

    let announcementsFilter =
        Filter.all
        |> Filter.notes
        |> Filter.authors [minterPubKey]
        |> Filter.limit 1

    let directMessagesFilter me =
        Filter.all
        |> Filter.encryptedMessages
        |> fun f -> { f with PubKeys = [me] }

    type ReadyState = {
        Credentials: Credential list
        Client: WabiSabiClient
    }

    type WaitingMoney = {
        Credentials: Credential list
        Client: WabiSabiClient
        ValidationData: CredentialsResponseValidation
    }

    type WaitingMoneyForPayment = {
        Credentials: Credential list
        Client: WabiSabiClient
        ValidationData: CredentialsResponseValidation
        Destination: AuthorId
    }

    type State =
        | Uninitialized
        | Ready of ReadyState
        | WaitingMoney of WaitingMoney
        | WaitingMoneyForPayment of WaitingMoneyForPayment

    let protocolHandlerLoop secret relay =
        let sendTo dst =
            sayTo relay secret dst

        let sendToMinter =
            sendTo minterPubKey

        let getUsableCredentials (credentials: Credential list) =
            credentials
            |> List.sortByDescending(fun x -> x.Value)
            |> List.mapi (fun i credential -> (i, credential))
            |> List.partition (fun (i, credential) -> i < ProtocolConstants.CredentialNumber)
            |> fun (toUse, rest) ->
                (List.map snd toUse, List.map snd rest)

        let worker =
            MailboxProcessor<EchashProtocolMessage>.Start (fun inbox ->
                let rec loop (state: State) = async {
                    let! msg = inbox.Receive()
                    let newState =
                        match msg with
                        | MinterMessage (MintParametersAnnouncement parameters) ->
                            match state with
                            | Uninitialized ->
                                let maxAmount = (2L <<< 32) - 1L
                                Ready {
                                    Client = WabiSabiClient(parameters, SecureRandom(), maxAmount)
                                    Credentials = []
                                }
                            | _ -> state
                        | MinterMessage (NewCredentialsMinted credentialResponse) ->
                            match state with
                            | WaitingMoney waitingMoneyState ->
                                let newCredentials =
                                    waitingMoneyState.Client.HandleResponse (credentialResponse, waitingMoneyState.ValidationData)
                                    |> Array.ofSeq
                                let c1, c2 = newCredentials[0], newCredentials[1]
                                let receivedAmount = c1.Value + c2.Value
                                let curBalance = waitingMoneyState.Credentials |>  Seq.sumBy (fun x -> x.Value)
                                log $"Received {receivedAmount}. New balance: {curBalance + receivedAmount}"
                                Ready {
                                    Client = waitingMoneyState.Client
                                    Credentials = c1 :: c2 :: waitingMoneyState.Credentials
                                }
                            | WaitingMoneyForPayment waitingMoneyState ->
                                let newCredentials =
                                    waitingMoneyState.Client.HandleResponse (credentialResponse, waitingMoneyState.ValidationData)
                                    |> Array.ofSeq
                                let credFormPayment, credChange = newCredentials[0], newCredentials[1]
                                let paymentJson =
                                    credFormPayment
                                    |> Encode.credential
                                    |> Encode.toString 0
                                sendTo (waitingMoneyState.Destination) paymentJson
                                let credentials = credChange :: waitingMoneyState.Credentials

                                log $"Sent {credFormPayment.Value}. New balance: {credentials |> List.sumBy (fun x -> x.Value)}"

                                Ready {
                                    Client = waitingMoneyState.Client
                                    Credentials = credentials
                                }
                            | _ -> state
                        | PayerMessage (NewPaymentReceived credential) ->
                            match state with
                            | Ready readyState ->
                                let receivedAmount = credential.Value
                                let curBalance = readyState.Credentials |>  Seq.sumBy (_.Value)
                                log $"Received {receivedAmount}. New balance: {curBalance + receivedAmount}"
                                Ready {
                                    Client = readyState.Client
                                    Credentials = credential :: readyState.Credentials
                                }
                            | _ -> state
                        | UserMessage (ConsolidateCredential) ->
                            match state with
                            | Ready readyState ->
                                let credentialsToUse, unSelectedCredential = getUsableCredentials (readyState.Credentials)
                                let amount = credentialsToUse |> Seq.sumBy (_.Value)
                                let credentialsRequest = readyState.Client.CreateRequest ([amount; 0], credentialsToUse, CancellationToken.None)
                                credentialsRequest.CredentialsRequest
                                |> CredentialRequest.serialize
                                |> sendToMinter
                                log "Consolidating..."
                                WaitingMoney {
                                    Client = readyState.Client
                                    Credentials = unSelectedCredential
                                    ValidationData = credentialsRequest.CredentialsResponseValidation
                                }
                            | _ -> state
                        | UserMessage (SendTo (amount, destination)) ->
                            match state with
                            | Ready readyState ->
                                let credentialsToUse, unSelectedCredential = getUsableCredentials (readyState.Credentials)
                                let availableAmount = credentialsToUse |> Seq.sumBy (_.Value)
                                let credentialsRequest = readyState.Client.CreateRequest ([amount; availableAmount - amount], credentialsToUse, CancellationToken.None)
                                credentialsRequest.CredentialsRequest
                                |> CredentialRequest.serialize
                                |> sendToMinter
                                WaitingMoneyForPayment {
                                    Client = readyState.Client
                                    Credentials = unSelectedCredential
                                    ValidationData = credentialsRequest.CredentialsResponseValidation
                                    Destination = destination
                                }
                            | _ -> state
                        | UserMessage (Buy amount) ->
                            match state with
                            | Ready readyState ->
                                let nullCredentialsToUse = readyState.Credentials |> List.filter (fun x -> x.Value = 0) |> List.take ProtocolConstants.CredentialNumber
                                let unSelectedCredentials = readyState.Credentials |> List.except nullCredentialsToUse
                                let credentialsRequest = readyState.Client.CreateRequest ([amount; 0], nullCredentialsToUse, CancellationToken.None)
                                credentialsRequest.CredentialsRequest
                                |> CredentialRequest.serialize
                                |> sendToMinter
                                WaitingMoney {
                                    Client = readyState.Client
                                    Credentials = unSelectedCredentials
                                    ValidationData = credentialsRequest.CredentialsResponseValidation
                                }
                            | _ -> state
                        | UserMessage (GetNullCredentials) ->
                            match state with
                            | Ready readyState ->
                                let zeroCredentials = readyState.Credentials |> List.filter (fun x -> x.Value = 0)
                                if List.length zeroCredentials <= 2 then
                                    let nullCredentialsRequest = readyState.Client.CreateRequestForZeroAmount ()
                                    let requestJson =
                                        nullCredentialsRequest.CredentialsRequest
                                        |> Encode.credentialsRequest
                                        |> Encode.toString 0
                                    sendToMinter requestJson
                                    WaitingMoney {
                                        Client = readyState.Client
                                        Credentials = readyState.Credentials
                                        ValidationData = nullCredentialsRequest.CredentialsResponseValidation
                                    }
                                else
                                    state
                            | _ -> state
                    return! loop(newState) }
                loop (Uninitialized) )
        worker.Post

    let processMinterAnnouncement (event: Event) =
        event.Content
        |> Decode.fromString Decode.issuerParameters

    let  processMinterDirectMessage secret (event: Event) =
        event
        |> Event.decryptDirectMessage secret
        |> Decode.fromString Decode.credentialsResponse

    let processPaymentDirectMessage secret (event: Event) =
        event
        |> Event.decryptDirectMessage secret
        |> Decode.fromString Decode.credential

    let dispatchProtocolHandler secret pushToProtocolHandler =
        commonProtocolHandler
            (fun subscriptionId event ->
                match subscriptionId with
                | MINT_ANNOUNCEMENTS_SUBSCRIPTION ->
                    match processMinterAnnouncement event with
                    | Ok minterParameters ->
                        pushToProtocolHandler (MinterMessage (MintParametersAnnouncement minterParameters))
                        pushToProtocolHandler (UserMessage GetNullCredentials)
                    | Error e -> log "Announcement was invalid"
                | DIRECT_MESSAGE_SUBSCRIPTION ->
                    let fromMinter = AuthorId.equals event.PubKey minterPubKey
                    if fromMinter then
                        match processMinterDirectMessage secret event with
                        | Ok issuanceResponse ->
                            pushToProtocolHandler (MinterMessage (NewCredentialsMinted issuanceResponse))
                            pushToProtocolHandler (UserMessage GetNullCredentials)
                        | Error e -> log e
                    else
                        match processPaymentDirectMessage secret event with
                        | Ok credential ->
                            pushToProtocolHandler (PayerMessage (NewPaymentReceived credential))
                            pushToProtocolHandler (UserMessage (ConsolidateCredential))
                            pushToProtocolHandler (UserMessage GetNullCredentials)
                        | Error e -> log e
                | _ -> log "The relay is crazy or what!?")

    let processUserCommandLoop pushToProtocolHandler =
        let rec loop () = async {
            Console.Write "# "
            let line = Console.ReadLine ()
            match line.Split [| ' ' |] with
            | [| "send"; amountStr; destinationStr |] ->
                let maybePubkey = Shareable.decodeNpub destinationStr
                let success, amount = Int32.TryParse amountStr
                match success, amount, maybePubkey with
                | true, amount, Some pubkey ->
                    pushToProtocolHandler (UserMessage (SendTo (amount, pubkey)))
                    log "Done"
                | false, _, _ ->
                    log "The amount is not a valid integer"
                | _ ->
                    log "public key is invalid"
            | [| "buy"; amountStr |] ->
                match Int32.TryParse amountStr with
                | true, amount ->
                    pushToProtocolHandler (UserMessage (Buy amount))
                    log $"Bought amount sabis from minter"
                | _ ->
                    log "The amount is not a valid integer"
            | [||] ->
                log ""
            | _ ->
                log "?"
            do! loop ()
        }
        loop ()

    let start uri secret pubkey = async {
        let! relay = connectToRelay uri
        let protocolHandlerPush = protocolHandlerLoop secret relay.publish
        let handleProtocolMessages = dispatchProtocolHandler secret protocolHandlerPush
        let readCommandFromUserLoop = processUserCommandLoop protocolHandlerPush
        let protocolHandlingLoop = relay.startListening handleProtocolMessages
        relay.subscribe MINT_ANNOUNCEMENTS_SUBSCRIPTION [announcementsFilter]
        relay.subscribe DIRECT_MESSAGE_SUBSCRIPTION [directMessagesFilter pubkey]

        do! (Async.Parallel [ protocolHandlingLoop; readCommandFromUserLoop ] |> Async.Ignore)
    }
