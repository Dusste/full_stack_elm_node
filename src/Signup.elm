module Signup exposing (Model, Msg, init, update, view)

import Credentials exposing (Token, encodeToken, storeSession, tokenDecoder)
import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode exposing (encode)
import Regex
import Utils exposing (buildErrorMessage, validEmail)


type alias Model =
    { signupCredentials : Credentials
    , errors : List CheckErrors
    , isLoading : Bool
    }


type CheckErrors
    = BadEmail String
    | BadPassword String
    | BadConfirmPassword String
    | PasswordMissmatch String
    | BadRequest String


initialModel : Model
initialModel =
    { signupCredentials = { email = "", password = "", confirmPassword = "" }
    , errors = []
    , isLoading = False
    }


type alias Credentials =
    { email : String
    , password : String
    , confirmPassword : String
    }


type Msg
    = StoreEmail String
    | StorePassword String
    | StoreConfirmPassword String
      -- | SignupError String
    | SignupSubmit Credentials
    | SignupDone (Result Http.Error Token)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreEmail email ->
            let
                oldCredentials =
                    model.signupCredentials

                updateCredentials =
                    { oldCredentials | email = email }
            in
            ( { model | signupCredentials = updateCredentials }, Cmd.none )

        StorePassword password ->
            let
                oldCredentials =
                    model.signupCredentials

                updateCredentials =
                    { oldCredentials | password = password }
            in
            ( { model | signupCredentials = updateCredentials }, Cmd.none )

        StoreConfirmPassword confirmPassword ->
            let
                oldCredentials =
                    model.signupCredentials

                updateCredentials =
                    { oldCredentials | confirmPassword = confirmPassword }
            in
            ( { model | signupCredentials = updateCredentials }, Cmd.none )

        SignupSubmit cred ->
            let
                errorsList =
                    sumOfErrors model
            in
            if List.isEmpty errorsList then
                ( { model | errors = [], isLoading = True }, submitSignup cred )

            else
                ( { model | errors = errorsList, isLoading = False }, Cmd.none )

        SignupDone (Ok token) ->
            let
                tokenValue =
                    encodeToken token
            in
            -- ( { model | errors = [] }, Cmd.none )
            ( { model | errors = [], isLoading = False }, storeSession <| Just <| encode 0 tokenValue )

        SignupDone (Err error) ->
            ( { model | errors = BadRequest (buildErrorMessage error) :: model.errors, isLoading = False }, Cmd.none )



-- SignupError errorMessage ->
--     ( { model | errors = List.append model.errors errorMessage }, Cmd.none )


sumOfErrors : Model -> List CheckErrors
sumOfErrors model =
    let
        isEmailEmpty =
            String.isEmpty model.signupCredentials.email

        isEmailValid =
            Regex.contains validEmail model.signupCredentials.email

        isPasswordEmpty =
            String.isEmpty model.signupCredentials.password

        isPasswordInvalid =
            String.length model.signupCredentials.password < 10

        isConfirmPasswordEmpty =
            String.isEmpty model.signupCredentials.confirmPassword

        isConfirmPasswordInvalid =
            String.length model.signupCredentials.confirmPassword < 10

        passwordsMissmatch =
            model.signupCredentials.password /= model.signupCredentials.confirmPassword
    in
    if isEmailEmpty then
        BadEmail "Email can't be empty" :: model.errors

    else if isEmailValid == False then
        BadEmail "Email is invalid" :: model.errors

    else if isPasswordEmpty then
        BadPassword "Password can't be empty" :: model.errors

    else if isPasswordInvalid then
        BadPassword "Password value is less then 10 characters" :: model.errors

    else if isConfirmPasswordEmpty then
        BadPassword "Confirm Password can't be empty" :: model.errors

    else if isConfirmPasswordInvalid then
        BadPassword "Confirm Password value is less then 10 characters" :: model.errors

    else if passwordsMissmatch then
        PasswordMissmatch "YO passwords doesn't match" :: model.errors

    else
        []


submitSignup : Credentials -> Cmd Msg
submitSignup credentials =
    Http.post
        { url = "/api/signup"
        , body = Http.jsonBody (credentialsEncoder credentials)
        , expect = Http.expectJson SignupDone tokenDecoder
        }


credentialsEncoder : Credentials -> Encode.Value
credentialsEncoder credentials =
    Encode.object
        [ ( "email", Encode.string credentials.email )
        , ( "password", Encode.string credentials.password )
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ h2 [] [ text "Signup" ]
        , ul []
            (List.map viewError model.errors)
        , Html.form []
            [ div []
                [ text "Email"
                , br [] []
                , input
                    [ type_ "text"
                    , onInput StoreEmail
                    , value model.signupCredentials.email
                    ]
                    []
                ]
            , br [] []
            , div []
                [ text "Password"
                , br [] []
                , input
                    [ type_ "password"
                    , onInput StorePassword
                    , value model.signupCredentials.password
                    ]
                    []
                ]
            , br [] []
            , div []
                [ text "Confirm Password"
                , br [] []
                , input
                    [ type_ "password"
                    , onInput StoreConfirmPassword
                    , value model.signupCredentials.confirmPassword
                    ]
                    []
                ]
            , br [] []
            , div []
                [ button
                    [ type_ "button"
                    , onClick (SignupSubmit model.signupCredentials)
                    ]
                    [ text "Sign up" ]
                ]
            , if model.isLoading then
                div [] [ text "LOADING ... " ]

              else
                div [] [ text "" ]
            ]
        ]


viewError : CheckErrors -> Html Msg
viewError checkErrors =
    case checkErrors of
        BadEmail err ->
            li [] [ p [] [ text err ] ]

        BadPassword err ->
            li [] [ p [] [ text err ] ]

        BadConfirmPassword err ->
            li [] [ p [] [ text err ] ]

        PasswordMissmatch err ->
            li [] [ p [] [ text err ] ]

        BadRequest err ->
            li [] [ p [] [ text err ] ]



-- enableSubmitButton : Credentials -> Attribute msg
-- enableSubmitButton cred =
--     let
--         passwordMatch =
--             cred.password
--                 == cred.confirmPassword
--         passwordHasValue =
--             String.length cred.password
--                 > 0
--         isEmailValid =
--             Regex.contains validEmail cred.email
--     in
--     if
--         passwordHasValue
--             && passwordMatch
--             && isEmailValid
--     then
--         attribute "enabled" ""
--     else
--         attribute "disabled" ""
-- port storeSession : String -> Cmd msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )
