module Chat exposing (..)

import Credentials exposing (Session, SocketMessageData, addUserToRoom, decodeSocketMessage, decodeTokenData, emitTyping, fromSessionToToken, fromTokenToString, sendMessageToSocket, userIdToString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Jwt
import Time
import Utils exposing (buildErrorMessage)


type alias Model =
    { message : String
    , receivedMessages : List SocketMessageData
    , error : Maybe String
    }


type CheckErrors
    = BadInput String
    | BadRequest String


type RoomId
    = Int


type Msg
    = StoreMessage String
    | MessageSubmit
    | MessageReceived SocketMessageData
    | SocketEstablished (Result Http.Error RoomId)
    | ChatMessages (Result Http.Error (List SocketMessageData))


unfoldMessageReceived : SocketMessageData -> Msg
unfoldMessageReceived socketData =
    MessageReceived socketData


initialModel : Model
initialModel =
    { message = ""
    , receivedMessages =
        []
    , error = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreMessage message ->
            ( { model | message = message }, emitTyping message )

        MessageSubmit ->
            ( { model | message = "" }, sendMessageToSocket model.message )

        MessageReceived message ->
            ( { model | receivedMessages = model.receivedMessages ++ [ message ] }, Cmd.none )

        SocketEstablished (Ok roomId) ->
            ( model, Cmd.none )

        SocketEstablished (Err err) ->
            ( { model | error = Just <| buildErrorMessage err }, Cmd.none )

        ChatMessages (Ok messages) ->
            ( { model | receivedMessages = model.receivedMessages ++ messages }, Cmd.none )

        ChatMessages (Err err) ->
            ( { model | error = Just <| buildErrorMessage err }, Cmd.none )


init : Session -> ( Model, Cmd Msg )
init session =
    case fromSessionToToken session of
        Just token ->
            case Jwt.decodeToken decodeTokenData <| fromTokenToString token of
                Ok resultTokenRecord ->
                    ( initialModel
                    , Cmd.batch
                        [ establishSocketConnection
                        , addUserToRoom
                            { userName = takeNameOrEmail resultTokenRecord.firstname resultTokenRecord.email
                            , userId = userIdToString resultTokenRecord.id
                            }
                        , fetchChatMessages
                        ]
                    )

                Err _ ->
                    ( initialModel, Cmd.none )

        Nothing ->
            ( initialModel, Cmd.none )


takeNameOrEmail : String -> String -> String
takeNameOrEmail name email =
    if String.isEmpty name then
        email

    else
        name


establishSocketConnection : Cmd Msg
establishSocketConnection =
    Http.get
        { url = "/api/socket?roomId=123"
        , expect = Http.expectJson SocketEstablished roomIdDecoder
        }


fetchChatMessages : Cmd Msg
fetchChatMessages =
    Http.get
        { url = "/api/messages"
        , expect = Http.expectJson ChatMessages (Decode.list decodeSocketMessage)
        }


roomIdDecoder : Decoder RoomId
roomIdDecoder =
    Decode.succeed Int


view : Model -> Html Msg
view model =
    div
        []
        [ h2 []
            [ text "Chat" ]
        , div []
            [ ul [] (List.map (\m -> viewMessage m) model.receivedMessages)
            ]
        , case model.error of
            Just err ->
                div [] [ text err ]

            Nothing ->
                div []
                    [ input
                        [ type_ "text"
                        , onInput StoreMessage
                        , value model.message
                        ]
                        []
                    , button
                        [ type_ "button"
                        , onClick MessageSubmit
                        ]
                        [ text "Send message" ]
                    ]
        ]


viewMessage : SocketMessageData -> Html Msg
viewMessage messageData =
    let
        hour =
            String.fromInt (Time.toHour Time.utc (Time.millisToPosix messageData.timestamp))

        minute =
            String.fromInt (Time.toMinute Time.utc (Time.millisToPosix messageData.timestamp))

        second =
            String.fromInt (Time.toSecond Time.utc (Time.millisToPosix messageData.timestamp))
    in
    li []
        [ div []
            [ div [] [ text <| messageData.name ++ ":" ]
            , div [] [ text <| messageData.data.message ]
            , div [] [ text <| hour ++ ":" ++ minute ++ ":" ++ second ]
            ]
        ]
