module Main exposing (..)

import Array
import Base64 exposing (decode)
import BinaryTree exposing (..)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Credentials
    exposing
        ( Session
        , UserId
        , VerificationString
        , decodeToSession
        , fromSessionToToken
        , fromTokenToString
        , logout
        , subscriptionChanges
        , unfoldProfileFromToken
        , userIdParser
        , userIdToString
        , verifictionStringParser
        )
import Home
import Html exposing (Html, a, div, footer, h1, li, nav, p, text, ul)
import Html.Attributes exposing (classList, href, style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Json.Decode as Decode exposing (Value)
import Login
import Minidenticons exposing (identicon)
import Profile
import Signup
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)
import Verification


type alias Model =
    { page : Page
    , key : Nav.Key
    , session : Session
    , openDropdown : Bool
    }


type Page
    = LoginPage Login.Model
    | SignupPage Signup.Model
    | ProfilePage Profile.Model
    | HomePage Home.Model
    | VerificationPage Verification.Model
    | NotFoundPage


type Route
    = Login
    | Signup
    | Profile UserId
    | Home
    | Verification VerificationString
    | NotFound


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotSignupMsg Signup.Msg
    | GotLoginMsg Login.Msg
    | GotProfileMsg Profile.Msg
    | GotHomeMsg Home.Msg
    | GotVerificationMsg Verification.Msg
    | GotSubscriptionChangeMsg Session
    | GetLogout
    | OpenDropdown


content : Model -> Html Msg
content model =
    case model.page of
        LoginPage loginModel ->
            Login.view loginModel
                |> Html.map GotLoginMsg

        SignupPage signupModel ->
            Signup.view signupModel
                |> Html.map GotSignupMsg

        ProfilePage profileModel ->
            Profile.view profileModel
                |> Html.map GotProfileMsg

        HomePage homeModel ->
            Home.view homeModel
                |> Html.map GotHomeMsg

        VerificationPage verificationModel ->
            Verification.view verificationModel
                |> Html.map GotVerificationMsg

        NotFoundPage ->
            p [] [ text "Page not found buddy -_- sorry" ]


view : Model -> Document Msg
view model =
    { title = "My elm app"
    , body =
        [ lazy viewHeader model
        , content model
        , viewFooter
        ]
    }


viewFooter : Html msg
viewFooter =
    footer []
        [ text "This is footer"
        ]


viewHeader : Model -> Html Msg
viewHeader { page, session, openDropdown, key } =
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
            nav []
                [ h1 [] [ a [ href "/" ] [ text "My elm app" ] ]
                , ul []
                    [ case maybeTokenData of
                        Just tokenData ->
                            let
                                decodedTokenData =
                                    decode tokenData
                            in
                            case decodedTokenData of
                                Err err ->
                                    text err

                                Ok encodedRecord ->
                                    case Decode.decodeString profileFromToken encodedRecord of
                                        Ok resultTokenRecord ->
                                            li
                                                [ classList
                                                    [ ( "active"
                                                      , isActive { link = Profile resultTokenRecord.id, page = page }
                                                      )
                                                    ]
                                                ]
                                                [ div []
                                                    [ ul
                                                        [ if openDropdown then
                                                            style "display" "block"

                                                          else
                                                            style "display" "none"
                                                        ]
                                                        [ li [] [ a [ href <| "/profile/" ++ userIdToString resultTokenRecord.id ] [ text "My profile" ] ], li [] [ text "option2" ], li [] [ text "option3" ] ]
                                                    , if String.isEmpty resultTokenRecord.firstname == False then
                                                        p
                                                            [ onClick OpenDropdown ]
                                                            [ text (resultTokenRecord.firstname ++ " ⌄")
                                                            , div [ style "width" "60px" ] [ identicon 50 50 resultTokenRecord.firstname ]
                                                            ]

                                                      else
                                                        p
                                                            [ onClick OpenDropdown ]
                                                            [ text
                                                                (resultTokenRecord.email ++ " ⌄")
                                                            , div [ style "width" "60px" ] [ identicon 50 50 resultTokenRecord.email ]
                                                            ]
                                                    ]
                                                ]

                                        Err _ ->
                                            li [] [ text "Profile" ]

                        Nothing ->
                            li [] [ a [ href "/" ] [ text "Home" ] ]
                    ]
                , li
                    []
                    [ a [ href "/", onClick GetLogout ] [ text "logout" ] ]
                ]

        Nothing ->
            nav []
                [ h1 [] [ a [ href "/" ] [ text "My elm app" ] ]
                , ul []
                    [ li
                        [ classList
                            [ ( "active"
                              , isActive { link = Login, page = page }
                              )
                            ]
                        ]
                        [ a [ href "/login" ] [ text "login" ] ]
                    , li
                        [ classList
                            [ ( "active"
                              , isActive { link = Signup, page = page }
                              )
                            ]
                        ]
                        [ a [ href "/signup" ] [ text "sign up" ] ]
                    , li
                        [ classList
                            [ ( "active"
                              , isActive { link = Home, page = page }
                              )
                            ]
                        ]
                        [ a [ href "/" ] [ text "home" ] ]
                    ]
                ]


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Login, LoginPage _ ) ->
            True

        ( Login, _ ) ->
            False

        ( Signup, SignupPage _ ) ->
            True

        ( Signup, _ ) ->
            False

        ( Profile _, ProfilePage _ ) ->
            True

        ( Profile _, _ ) ->
            False

        ( Home, HomePage _ ) ->
            True

        ( Home, _ ) ->
            False

        ( Verification _, VerificationPage _ ) ->
            True

        ( Verification _, _ ) ->
            False

        ( NotFound, _ ) ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            let
                newPage =
                    urlToPage url model.session
            in
            initCurrentPage ( url, { model | page = newPage }, Cmd.none )

        GotLoginMsg loginMsg ->
            case model.page of
                LoginPage loginModel ->
                    let
                        ( loginModelFromLogin, loginMsgFromLogin ) =
                            Login.update loginMsg loginModel
                    in
                    ( { model | page = LoginPage loginModelFromLogin }, Cmd.map GotLoginMsg loginMsgFromLogin )

                _ ->
                    ( model, Cmd.none )

        GotProfileMsg profileMsg ->
            case model.page of
                ProfilePage profileModel ->
                    let
                        ( profileModelFromProfile, profileMsgFromProfile ) =
                            Profile.update profileMsg profileModel
                    in
                    ( { model | page = ProfilePage profileModelFromProfile }, Cmd.map GotProfileMsg profileMsgFromProfile )

                _ ->
                    ( model, Cmd.none )

        GotHomeMsg homeMsg ->
            case model.page of
                HomePage homeModel ->
                    let
                        ( homeModelFromHome, homeMsgFromHome ) =
                            Home.update homeMsg homeModel
                    in
                    ( { model | page = HomePage homeModelFromHome }, Cmd.map GotHomeMsg homeMsgFromHome )

                _ ->
                    ( model, Cmd.none )

        GotVerificationMsg verificationMsg ->
            case model.page of
                VerificationPage verificationModel ->
                    let
                        ( verificationModelFromVerification, verificationMsgFromVerification ) =
                            Verification.update verificationMsg verificationModel
                    in
                    ( { model | page = VerificationPage verificationModelFromVerification }, Cmd.map GotVerificationMsg verificationMsgFromVerification )

                _ ->
                    ( model, Cmd.none )

        GotSignupMsg signupMsg ->
            case model.page of
                SignupPage signupModel ->
                    let
                        ( signupModelFromSignup, signupMsgFromSignup ) =
                            Signup.update signupMsg signupModel
                    in
                    ( { model | page = SignupPage signupModelFromSignup }, Cmd.map GotSignupMsg signupMsgFromSignup )

                _ ->
                    ( model, Cmd.none )

        GotSubscriptionChangeMsg session ->
            ( { model | session = session }
            , case fromSessionToToken session of
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
                                    Nav.pushUrl model.key "/login"

                                Ok encodedRecord ->
                                    case Decode.decodeString profileFromToken encodedRecord of
                                        Ok resultTokenRecord ->
                                            Nav.pushUrl model.key ("/profile/" ++ userIdToString resultTokenRecord.id)

                                        Err _ ->
                                            Nav.pushUrl model.key "/login"

                        Nothing ->
                            Nav.pushUrl model.key "/login"

                Nothing ->
                    Nav.pushUrl model.key "/login"
            )

        GetLogout ->
            ( model, logout )

        OpenDropdown ->
            ( { model | openDropdown = not model.openDropdown }, Cmd.none )


matchRoute : Parser (Route -> a) a
matchRoute =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Profile (s "profile" </> userIdParser)
        , Parser.map Signup (s "signup")
        , Parser.map Verification (s "verify-email" </> verifictionStringParser)
        ]


urlToPage : Url -> Session -> Page
urlToPage url session =
    case Parser.parse matchRoute url of
        Just Login ->
            LoginPage (Tuple.first (Login.init ()))

        Just Signup ->
            SignupPage (Tuple.first (Signup.init ()))

        Just (Profile _) ->
            ProfilePage (Tuple.first (Profile.init session))

        Just Home ->
            HomePage (Tuple.first (Home.init ()))

        Just (Verification _) ->
            VerificationPage (Tuple.first (Verification.init session url.path))

        Just NotFound ->
            NotFoundPage

        Nothing ->
            NotFoundPage



-- pageToRoute : Page -> Route
-- pageToRoute page =
--     case page of
--         SignupPage _ ->
--             Signup
--         LoginPage _ ->
--             Login
--         ProfilePage _ ->
--             Profile Nothing
--         HomePage _ ->
--             Home
--         VerificationPage _ ->
--             Verification (s "verify-email" </> verifictionStringParser)
--         NotFoundPage ->
--             NotFound


initCurrentPage : ( Url, Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( url, model, existingCmds ) =
    case model.page of
        NotFoundPage ->
            ( { model | page = NotFoundPage }, Cmd.none )

        LoginPage _ ->
            let
                ( pageModel, pageCmds ) =
                    Login.init ()

                -- Because Main doesn’t know anything about the page specific messages, it needs to map them to one of the data constructors from its own Msg type using the Cmd.map function
            in
            ( { model | page = LoginPage pageModel }, Cmd.map GotLoginMsg pageCmds )

        SignupPage _ ->
            let
                ( pageModel, pageCmds ) =
                    Signup.init ()
            in
            ( { model | page = SignupPage pageModel }, Cmd.map GotSignupMsg pageCmds )

        HomePage _ ->
            let
                ( pageModel, pageCmds ) =
                    Home.init ()
            in
            ( { model | page = HomePage pageModel }, Cmd.map GotHomeMsg pageCmds )

        VerificationPage _ ->
            let
                ( pageModel, pageCmds ) =
                    Verification.init model.session url.path
            in
            ( { model | page = VerificationPage pageModel }, Cmd.map GotVerificationMsg pageCmds )

        ProfilePage _ ->
            let
                ( pageModel, pageCmds ) =
                    Profile.init model.session
            in
            ( { model | page = ProfilePage pageModel }, Cmd.map GotProfileMsg pageCmds )


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session =
            decodeToSession key flags

        model =
            { page = urlToPage url session, key = key, session = session, openDropdown = False }
    in
    initCurrentPage ( url, model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    subscriptionChanges GotSubscriptionChangeMsg model.key


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
