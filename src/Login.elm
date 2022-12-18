module Login exposing (Model, Msg, init, update, view)

import Browser
import Credentials exposing (Token, encodeToken, storeSession, tokenDecoder)
import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, encode)
import Regex exposing (Regex)
import Utils exposing (buildErrorMessage, validEmail)


type alias Model =
    { loginCredentials : Credentials
    , errors : List CheckErrors
    }


type CheckErrors
    = BadEmail String
    | BadPassword String
    | BadRequest String


initialModel : Model
initialModel =
    { loginCredentials = { email = "", password = "" }
    , errors = []
    }


type alias Credentials =
    { email : String
    , password : String
    }


type Msg
    = StoreEmail String
    | StorePassword String
    | LoginSubmit Credentials
    | LoginDone (Result Http.Error Token)



-- | GetShit (WebData Shit)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreEmail email ->
            let
                oldCredentials =
                    model.loginCredentials

                updateCredentials =
                    { oldCredentials | email = email }
            in
            ( { model | loginCredentials = updateCredentials }, Cmd.none )

        StorePassword password ->
            let
                oldCredentials =
                    model.loginCredentials

                updateCredentials =
                    { oldCredentials | password = password }
            in
            ( { model | loginCredentials = updateCredentials }, Cmd.none )

        -- GetShit newShit ->
        --     ( { model | shit = newShit }, Cmd.none )
        LoginSubmit cred ->
            let
                errorsList =
                    sumOfErrors model
            in
            if List.isEmpty errorsList then
                ( { model | errors = [] }, submitLogin model.loginCredentials )

            else
                ( { model | errors = errorsList }, Cmd.none )

        LoginDone (Ok token) ->
            let
                tokenValue =
                    encodeToken token
            in
            ( { model | errors = [] }, storeSession <| Just <| encode 0 tokenValue )

        LoginDone (Err error) ->
            ( { model | errors = BadRequest (buildErrorMessage error) :: model.errors }, Cmd.none )


sumOfErrors : Model -> List CheckErrors
sumOfErrors model =
    let
        isEmailEmpty =
            String.isEmpty model.loginCredentials.email

        isEmailValid =
            Regex.contains validEmail model.loginCredentials.email

        isPasswordEmpty =
            String.isEmpty model.loginCredentials.password

        isPasswordInvalid =
            String.length model.loginCredentials.password < 10
    in
    if isEmailEmpty then
        BadEmail "Email can't be empty" :: model.errors

    else if isEmailValid == False then
        BadEmail "Email is invalid" :: model.errors

    else if isPasswordEmpty then
        BadPassword "Password can't be empty" :: model.errors

    else if isPasswordInvalid then
        BadPassword "Password value is less then 10 characters" :: model.errors

    else
        []


submitLogin : Credentials -> Cmd Msg
submitLogin credentials =
    Http.post
        { url = "/.netlify/functions/login-api"
        , body = Http.jsonBody (credentialsEncoder credentials)
        , expect = Http.expectJson LoginDone tokenDecoder
        }


credentialsEncoder : Credentials -> Encode.Value
credentialsEncoder credentials =
    Encode.object
        [ ( "email", Encode.string credentials.email )
        , ( "password", Encode.string credentials.password )
        ]



-- shitDecoder : Decoder Shit
-- shitDecoder =
--     Decode.succeed Shit
--         |> required "firstName" string
--         |> required "lastName" string
--         |> required "occupation" string


view : Model -> Html Msg
view model =
    div
        []
        [ h2 [] [ text "Login" ]
        , ul []
            (List.map viewError model.errors)
        , Html.form []
            [ div []
                [ text "Email"
                , br [] []
                , input [ type_ "text", onInput StoreEmail, value model.loginCredentials.email ] []
                ]
            , br [] []
            , div []
                [ text "Password"
                , br [] []
                , input [ type_ "password", onInput StorePassword, value model.loginCredentials.password ] []
                ]
            , br [] []
            , div []
                [ button [ type_ "button", onClick (LoginSubmit model.loginCredentials) ]
                    [ text "Sign in" ]
                ]
            ]

        -- , viewShit model.shit
        -- , viewError model.errorMessage
        ]


viewError : CheckErrors -> Html Msg
viewError checkErrors =
    case checkErrors of
        BadEmail err ->
            li [] [ p [] [ text err ] ]

        BadPassword err ->
            li [] [ p [] [ text err ] ]

        BadRequest err ->
            li [] [ p [] [ text err ] ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }
