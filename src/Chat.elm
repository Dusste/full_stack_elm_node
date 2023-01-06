module Chat exposing (..)

import Credentials exposing (Session, SocketMessageData, decodeTokenData, fromSessionToToken, fromTokenToString, initiateSocketChannel, sendMessageToSocket, userIdToString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Jwt


type alias Model =
    { message : String, receivedSocketData : SocketMessageData }


type CheckErrors
    = BadInput String
    | BadRequest String


type Msg
    = StoreMessage String
    | MessageSubmit


initialModel : Model
initialModel =
    { message = ""
    , receivedSocketData =
        { name = ""
        , id = ""
        , clientId = ""
        , connectionId = ""
        , timestamp = 0
        , data =
            { message = ""
            }
        }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreMessage message ->
            ( { model | message = message }, Cmd.none )

        MessageSubmit ->
            ( { model | message = "" }, sendMessageToSocket model.message )


init : Session -> Maybe SocketMessageData -> ( Model, Cmd Msg )
init session maybeSocket =
    case fromSessionToToken session of
        Just token ->
            case Jwt.decodeToken decodeTokenData <| fromTokenToString token of
                Ok resultTokenRecord ->
                    case maybeSocket of
                        Just receivedSocketData ->
                            ( { initialModel | receivedSocketData = receivedSocketData }, initiateSocketChannel <| userIdToString resultTokenRecord.id )

                        Nothing ->
                            ( initialModel, initiateSocketChannel <| userIdToString resultTokenRecord.id )

                Err _ ->
                    ( initialModel, Cmd.none )

        Nothing ->
            ( initialModel, Cmd.none )



-- Just receivedSocketData )
-- { initialModel | receivedSocketData = receivedSocketData }
-- ( Nothing, Nothing ) ->
--     ( initialModel, Cmd.none )
-- ( Just _, Nothing ) ->
--     ( initialModel, Cmd.none )
-- ( Nothing, Just _ ) ->
--     ( initialModel, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        [ h2 []
            [ text "Chat" ]
        , div []
            [ ul [] [ text model.receivedSocketData.data.message ]
            ]
        , Html.form []
            [ div []
                [ text "SomeUser:"
                , br [] []
                , input
                    [ type_ "text"
                    , onInput StoreMessage
                    , value model.message
                    ]
                    []
                ]
            , button
                [ type_ "button"
                , onClick MessageSubmit
                ]
                [ text "Send message" ]
            ]
        ]
