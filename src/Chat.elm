module Chat exposing (..)

import Credentials exposing (Session, SocketMessageData, addUserToRoom, decodeSocketMessage, decodeTokenData, emitTyping, fromSessionToToken, fromTokenToString, sendMessageToSocket, userIdToString)
import Css
import Css.Global
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Event
import Http
import Json.Decode as Decode exposing (Decoder)
import Jwt
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
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
    Html.div
        []
        [ Html.h2 []
            [ text "Chat" ]
        , Html.div []
            [ Html.ul [] (List.map (\m -> viewMessage m) model.receivedMessages)
            ]
        , case model.error of
            Just err ->
                Html.div [] [ text err ]

            Nothing ->
                Html.div []
                    [ Html.input
                        [ Attr.type_ "text"
                        , Event.onInput StoreMessage
                        , Attr.value model.message
                        ]
                        []
                    , Html.button
                        [ Attr.type_ "button"
                        , Event.onClick MessageSubmit
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
    Html.li []
        [ Html.div []
            [ Html.div [] [ text <| messageData.name ++ ":" ]
            , Html.div [] [ text <| messageData.data.message ]
            , Html.div [] [ text <| hour ++ ":" ++ minute ++ ":" ++ second ]
            ]
        ]
