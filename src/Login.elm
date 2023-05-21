module Login exposing (Model, Msg, init, update, view)

import Credentials exposing (Token, encodeToken, storeSession, tokenDecoder)
import Css
import Css.Global
import GlobalStyles as Gs
import Helpers exposing (buildErrorMessage, loadingElement, validEmail)
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


type alias Model =
    { loginCredentials : Credentials
    , errors : List CheckErrors
    , isLoading : Bool
    }


type CheckErrors
    = BadEmail String
    | BadPassword String
    | BadRequest String


initialModel : Model
initialModel =
    { loginCredentials = { email = "", password = "" }
    , errors = []
    , isLoading = False
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
    | HideError


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

        LoginSubmit cred ->
            let
                errorsList =
                    sumOfErrors model
            in
            if List.isEmpty errorsList then
                ( { model | errors = [], isLoading = True }, submitLogin cred )

            else
                ( { model | errors = errorsList, isLoading = False }, Process.sleep 4000 |> Task.perform (\_ -> HideError) )

        HideError ->
            ( { model | errors = model.errors |> List.drop 1 }, Cmd.none )

        LoginDone (Ok token) ->
            let
                tokenValue =
                    encodeToken token
            in
            ( { model | errors = [], isLoading = False }, storeSession <| Just <| encode 0 tokenValue )

        LoginDone (Err error) ->
            ( { model | errors = BadRequest (buildErrorMessage error) :: model.errors, isLoading = False }, Cmd.none )


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
        { url = "/api/login"
        , body = Http.jsonBody (credentialsEncoder credentials)
        , expect = Http.expectJson LoginDone tokenDecoder
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
        [ Html.h2 [ Attr.css [ Tw.text_3xl ] ] [ text "Login" ]
        , Html.form [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ] ]
            [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                [ Html.label [] [ text "Email" ]
                , Html.input [ Attr.css Gs.inputStyle, type_ "text", onInput StoreEmail, value model.loginCredentials.email ] []
                ]
            , Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                [ Html.label [ Attr.css [] ] [ text "Password" ]
                , Html.input [ Attr.css Gs.inputStyle, type_ "password", onInput StorePassword, value model.loginCredentials.password ] []
                ]
            , Html.div [ Attr.css [ Tw.flex, Tw.gap_3, Tw.items_center ] ]
                [ Html.button [ Attr.css Gs.buttonStyle, type_ "button", onClick (LoginSubmit model.loginCredentials) ]
                    [ text "Sign in" ]
                , if model.isLoading then
                    loadingElement

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

        BadRequest err ->
            Html.li [ Attr.css [ Tw.text_color Tw.red_400 ] ] [ Html.p [] [ text err ] ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )
