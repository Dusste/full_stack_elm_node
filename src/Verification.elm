module Verification exposing (Model, Msg, init, update, view)

import Array
import Base64 exposing (decode)
import Credentials exposing (Session, Token, addHeader, encodeToken, fromSessionToToken, fromTokenToString, isSessionValid, storeSession, tokenDecoder, unfoldProfileFromToken)
import Html exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode exposing (encode)
import Process
import Task


type alias Model =
    { userState : UserState
    }


type UserState
    = VerificationPending
    | VerificationFail
    | VerificationDone
    | Verified
    | Intruder


type Msg
    = VerifyApiCallStart Session
    | VerifyDone (Result Http.Error Token)
    | TokenToLS Token


init : Session -> ( Model, Cmd Msg )
init session =
    case fromSessionToToken session of
        Just token ->
            let
                -- unwrap profile data only if you have token
                profileFromToken =
                    unfoldProfileFromToken token

                -- unwrap token string only if you have token
                tokenString : String
                tokenString =
                    fromTokenToString token

                profile : List String
                profile =
                    String.split "." tokenString

                maybeTokenData : Maybe String
                maybeTokenData =
                    Array.fromList profile |> Array.get 1
            in
            case maybeTokenData of
                Just tokenData ->
                    let
                        decodedTokenData =
                            decode tokenData
                    in
                    case decodedTokenData of
                        Err _ ->
                            ( { userState = Intruder
                              }
                            , Cmd.none
                            )

                        Ok encodedRecord ->
                            case Decode.decodeString profileFromToken encodedRecord of
                                Ok resultTokenRecord ->
                                    if not resultTokenRecord.isverified then
                                        ( { userState = VerificationPending
                                          }
                                        , apiCallAfterSomeTime session VerifyApiCallStart
                                        )

                                    else
                                        ( { userState = Verified
                                          }
                                        , Cmd.none
                                        )

                                Err _ ->
                                    ( { userState = Intruder
                                      }
                                    , Cmd.none
                                    )

                Nothing ->
                    ( { userState = Intruder
                      }
                    , Cmd.none
                    )

        Nothing ->
            ( { userState = Intruder
              }
            , Cmd.none
            )


apiCallAfterSomeTime : Session -> (Session -> Msg) -> Cmd Msg
apiCallAfterSomeTime session toMsg =
    Process.sleep 5000
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
        VerifyApiCallStart session ->
            ( model, apiCallToVerify session )

        VerifyDone (Ok token) ->
            ( { model | userState = VerificationDone }
            , Process.sleep 5000
                |> Task.perform (\_ -> TokenToLS token)
            )

        VerifyDone (Err _) ->
            ( { model
                | userState = VerificationFail
              }
            , Cmd.none
            )

        TokenToLS token ->
            let
                tokenValue =
                    encodeToken token
            in
            ( model
            , storeSession <|
                Just <|
                    encode 0 tokenValue
            )


view : Model -> Html Msg
view model =
    case model.userState of
        VerificationPending ->
            div []
                [ h2 [] [ text "Give us a moment to verify your account ! " ]
                , p [] [ text "Soon you will have access to a all profile features" ]
                , p [] [ text "LOADING..." ]
                ]

        VerificationDone ->
            div []
                [ h2 [] [ text "Thanks for verifying your email ! " ]
                , p [] [ text "Now you will be redirected to your profile page and have full access to all app's features" ]
                , p [] [ text "LOADING..." ]
                ]

        VerificationFail ->
            div []
                [ h2 [] [ text "UPS seems that something is off !" ]
                , p [] [ text "Try to re-login or refresh the page" ]
                ]

        Verified ->
            div []
                [ h2 [] [ text "HMMm seems that you're already verified !" ]
                , p [] [ text "Please proceed to you profile" ]
                ]

        Intruder ->
            div []
                [ h2 [] [ text "You are not logged in !" ]
                , p [] [ text "Please proceed to login" ]
                ]
