module Profile exposing (..)

import Array
import Base64 exposing (decode)
import Credentials exposing (Session, Token, UnwrappedTokenData, addHeader, decodeTokenData, emptyUserId, encodeToken, fromSessionToToken, fromTokenToString, logout, profileDataDecoder, profileDataEncoder, storeSession, tokenDecoder, unfoldProfileFromToken, unwrappedTokenDataEncoder)
import Debug exposing (toString)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (alt, checked, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder, Value, bool, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, encode)
import RemoteData exposing (WebData)
import Task


type alias Model =
    { profile : UnwrappedTokenData
    , errors : List CheckErrors
    , session : Maybe Session
    , imageFile : Maybe String
    }


type CheckErrors
    = BadInput String
    | BadRequest String



-- RemoteData.Failure httpError ->
--     viewError (Just (buildErrorMessage httpError))


type Msg
    = StoreFirstName String
      -- | StoreLastName String
    | StoreVerified Bool
      -- | StoreAdmin Bool
    | ProfileSubmit (Maybe Session) UnwrappedTokenData
    | ProfileDone (Result Http.Error Token)
    | FileRequest
    | FileRequestDone File
    | FileRead String


initialModel : Model
initialModel =
    { profile =
        { firstname = ""
        , email = ""
        , id = emptyUserId
        , isverified = False
        , iat = 0
        , exp = 0
        }
    , errors = []
    , session = Nothing
    , imageFile = Nothing
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        maybeToken =
            fromSessionToToken session
    in
    case maybeToken of
        Just token ->
            let
                profileFromToken =
                    unfoldProfileFromToken token

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
                        resultTokenData =
                            decode tokenData
                    in
                    case resultTokenData of
                        Err err ->
                            ( initialModel
                            , Cmd.none
                            )

                        Ok encodedRecord ->
                            case Decode.decodeString profileFromToken encodedRecord of
                                Ok profileData ->
                                    ( { initialModel | profile = profileData, session = Just session }
                                    , Cmd.none
                                    )

                                Err _ ->
                                    ( initialModel
                                    , Cmd.none
                                    )

                Nothing ->
                    ( initialModel, Cmd.none )

        Nothing ->
            ( initialModel
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        []
        [ h2 [] [ text ("Hello" ++ model.profile.firstname) ]
        , Html.form []
            [ div []
                [ text "First Name"
                , br [] []
                , input
                    [ type_ "text"
                    , onInput StoreFirstName
                    , value model.profile.firstname
                    ]
                    []
                ]
            , br [] []

            -- , div []
            --     [ text "Email"
            --     , br [] []
            --     , input
            --         [ type_ "text"
            --         , onInput StoreLastName
            --         , value model.profile.email
            --         ]
            --         []
            --     ]
            -- , br [] []
            , div []
                [ text "Verified"
                , br [] []
                , input
                    [ type_ "checkbox"
                    , checked model.profile.isverified
                    , onClick (StoreVerified <| not model.profile.isverified)
                    ]
                    []
                ]
            , br [] []
            , div []
                [ text "Upload a avatar"
                , br [] []
                , input [ type_ "file", onClick FileRequest ] []
                ]
            , br [] []
            , div []
                [ text "Your avatar"
                , br [] []
                , img
                    [ src
                        (case model.imageFile of
                            Just fileString ->
                                fileString

                            Nothing ->
                                ""
                        )
                    ]
                    []
                ]

            -- , div []
            --     [ text "Admin"
            --     , br [] []
            --     , input
            --         [ type_ "checkbox"
            --         , onClick (StoreAdmin (not model.profile.isadmin))
            --         ]
            --         []
            --     ]
            , br [] []
            , div []
                [ button
                    [ type_ "button"
                    , onClick (ProfileSubmit model.session model.profile)
                    ]
                    [ text "Submit" ]
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreFirstName firstName ->
            let
                oldProfile =
                    model.profile

                updateProfile =
                    { oldProfile | firstname = firstName }
            in
            ( { model | profile = updateProfile }, Cmd.none )

        -- StoreLastName lastName ->
        --     let
        --         oldProfile =
        --             model.profile
        --         updateProfile =
        --             { oldProfile | lastname = lastName }
        --     in
        --     ( { model | profile = updateProfile }, Cmd.none )
        StoreVerified isVerified ->
            let
                oldProfile =
                    model.profile

                updateProfile =
                    { oldProfile | isverified = isVerified }
            in
            ( { model | profile = updateProfile }, Cmd.none )

        -- StoreAdmin isAdmin ->
        --     let
        --         oldProfile =
        --             model.profile
        --         updateProfile =
        --             { oldProfile | isadmin = isAdmin }
        --     in
        --     ( { model | profile = updateProfile }, Cmd.none )
        ProfileSubmit session cred ->
            ( model, submitProfile session cred )

        ProfileDone (Ok token) ->
            let
                tokenValue =
                    encodeToken token
            in
            ( model, storeSession <| Just <| encode 0 tokenValue )

        ProfileDone (Err error) ->
            ( { model | errors = [ BadRequest "Wrong !" ] }
            , case error of
                Http.BadStatus statusCode ->
                    if statusCode == 401 then
                        logout

                    else
                        Cmd.none

                _ ->
                    Cmd.none
            )

        FileRequest ->
            ( model, Select.file [ "image/png" ] FileRequestDone )

        FileRequestDone file ->
            ( model, Task.perform FileRead (File.toString file) )

        FileRead imageFileString ->
            ( { model | imageFile = Just imageFileString }, Cmd.none )


submitProfile : Maybe Session -> UnwrappedTokenData -> Cmd Msg
submitProfile maybeSession credentials =
    case maybeSession of
        Just session ->
            let
                maybeToken =
                    fromSessionToToken session
            in
            case maybeToken of
                Just token ->
                    Http.request
                        { method = "PUT"
                        , headers = [ addHeader token ]
                        , url = "/.netlify/functions/profile-put-api"
                        , body = Http.jsonBody (unwrappedTokenDataEncoder credentials)
                        , expect = Http.expectJson ProfileDone tokenDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }

                Nothing ->
                    Cmd.none

        Nothing ->
            Cmd.none
