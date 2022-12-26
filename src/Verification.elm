module Verification exposing (Model, Msg, init, update, view)

import Credentials exposing (Session, Token, addHeader, encodeToken, fromSessionToToken, isSessionValid, storeSession, tokenDecoder)
import Html exposing (..)
import Http
import Json.Encode exposing (encode)
import Process
import Task
import Utils exposing (buildErrorMessage)


type alias Model =
    { session : Session
    , errorMessage : String
    , verificationDone : Bool
    }


type Msg
    = VerifyApiCall Session
    | VerifyDone (Result Http.Error Token)


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , errorMessage = ""
      , verificationDone = False
      }
    , apiCallAfterSomeTime session VerifyApiCall
    )


apiCallAfterSomeTime : Session -> (Session -> Msg) -> Cmd Msg
apiCallAfterSomeTime session toMsg =
    Process.sleep 10000
        |> Task.perform
            (\_ -> toMsg session)


apiCallToVerify : Session -> Cmd Msg
apiCallToVerify session =
    case fromSessionToToken session of
        Just token ->
            Http.request
                { method = "PUT"
                , headers = [ addHeader token ]
                , url = "/.netlify/functions/verify-put-api"
                , expect = Http.expectJson VerifyDone tokenDecoder
                , body = Http.emptyBody
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VerifyApiCall session ->
            ( model, apiCallToVerify session )

        VerifyDone (Ok token) ->
            let
                tokenValue =
                    encodeToken token
            in
            ( { model | verificationDone = True }, storeSession <| Just <| encode 0 tokenValue )

        VerifyDone (Err error) ->
            ( { model | errorMessage = buildErrorMessage error, verificationDone = False }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 []
            [ text "Your profile will be verified shortly... " ]
        , div
            []
            [ if model.verificationDone then
                div []
                    [ h2 [] [ text "Thanks for verifying email ! " ]
                    , p [] [ text "Now you will be redirected to your profile page and have full access to all app's features" ]
                    , p [] [ text model.errorMessage ]
                    ]

              else
                div
                    []
                    [ p [] [ text "Please login first" ] ]
            ]
        ]
