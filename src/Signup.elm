module Signup exposing (Model, Msg, init, update, view)

import Credentials exposing (Token, encodeToken, storeSession, tokenDecoder)
import Css
import Css.Global
import GlobalStyles as Gs
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr exposing (type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode exposing (encode)
import Process
import Regex
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task
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
    | HideError


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
                ( { model | errors = errorsList, isLoading = False }, Process.sleep 4000 |> Task.perform (\_ -> HideError) )

        HideError ->
            ( { model | errors = model.errors |> List.drop 1 }, Cmd.none )

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
        model.errors ++ [ BadEmail "Email can't be empty" ]

    else if isEmailValid == False then
        model.errors ++ [ BadEmail "Email is invalid" ]

    else if isPasswordEmpty then
        model.errors ++ [ BadPassword "Password can't be empty" ]

    else if isPasswordInvalid then
        model.errors ++ [ BadPassword "Password value is less then 10 characters" ]

    else if isConfirmPasswordEmpty then
        model.errors ++ [ BadPassword "Confirm Password can't be empty" ]

    else if isConfirmPasswordInvalid then
        model.errors ++ [ BadPassword "Confirm Password value is less then 10 characters" ]

    else if passwordsMissmatch then
        model.errors ++ [ PasswordMissmatch "YO passwords doesn't match" ]

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
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.md [ Tw.m_20 ] ] ]
        [ Html.h2 [ Attr.css [ Tw.text_3xl ] ] [ text "Signup" ]
        , Html.form [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ] ]
            [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                [ text "Email"
                , Html.input
                    [ Attr.css Gs.inputStyle
                    , type_ "text"
                    , onInput StoreEmail
                    , value model.signupCredentials.email
                    ]
                    []
                ]
            , Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                [ text "Password"
                , Html.input
                    [ Attr.css Gs.inputStyle
                    , type_ "password"
                    , onInput StorePassword
                    , value model.signupCredentials.password
                    ]
                    []
                ]
            , Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                [ text "Confirm Password"
                , Html.input
                    [ Attr.css Gs.inputStyle
                    , type_ "password"
                    , onInput StoreConfirmPassword
                    , value model.signupCredentials.confirmPassword
                    ]
                    []
                ]
            , Html.div [ Attr.css [ Tw.flex, Tw.gap_3, Tw.items_center ] ]
                [ Html.button
                    [ Attr.css Gs.buttonStyle
                    , type_ "button"
                    , onClick (SignupSubmit model.signupCredentials)
                    ]
                    [ text "Sign up" ]
                , if model.isLoading then
                    Html.div [ Attr.css [ Tw.relative, Tw.h_5, Tw.w_5, Tw.flex ] ]
                        [ Html.span
                            [ Attr.css [ Tw.animate_ping, Tw.absolute, Tw.inline_flex, Tw.h_full, Tw.w_full, Tw.rounded_full, Tw.bg_color Tw.sky_400, Tw.opacity_75 ] ]
                            []
                        , Html.span [ Attr.css [ Tw.relative, Tw.inline_flex, Tw.rounded_full, Tw.h_5, Tw.w_5, Tw.bg_color Tw.sky_500 ] ] []
                        ]

                  else
                    text ""
                ]
            ]
        , Html.ul []
            (List.map viewError model.errors)
        ]


viewError : CheckErrors -> Html Msg
viewError checkErrors =
    case checkErrors of
        BadEmail err ->
            Html.li [ Attr.css [ Tw.text_color Tw.red_400 ] ] [ Html.p [] [ text err ] ]

        BadPassword err ->
            Html.li [ Attr.css [ Tw.text_color Tw.red_400 ] ] [ Html.p [] [ text err ] ]

        BadConfirmPassword err ->
            Html.li [ Attr.css [ Tw.text_color Tw.red_400 ] ] [ Html.p [] [ text err ] ]

        PasswordMissmatch err ->
            Html.li [ Attr.css [ Tw.text_color Tw.red_400 ] ] [ Html.p [] [ text err ] ]

        BadRequest err ->
            Html.li [ Attr.css [ Tw.text_color Tw.red_400 ] ] [ Html.p [] [ text err ] ]



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
