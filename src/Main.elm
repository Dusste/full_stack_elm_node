module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Chat
import Credentials
    exposing
        ( Session
        , SocketMessageData
        , UserId
        , VerificationString
        , decodeToSession
        , decodeTokenData
        , fromSessionToToken
        , fromTokenToString
        , imageStringToMaybeString
        , logout
        , socketMessageChanges
        , subscriptionChanges
        , userIdParser
        , userIdToString
        , verifictionStringParser
        )
import Home
import Html exposing (Html, a, div, footer, h1, img, li, nav, p, span, text, ul)
import Html.Attributes exposing (classList, href, src, style, width)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Json.Decode exposing (Value)
import Jwt
import Login
import Minidenticons exposing (identicon)
import Profile
import Signup
import Task
import Time
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
    | ChatPage Chat.Model
    | VerificationPage Verification.Model
    | NotFoundPage


type Route
    = Login
    | Signup
    | Profile UserId
    | Home
    | Chat
    | Verification VerificationString
    | NotFound


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotSignupMsg Signup.Msg
    | GotLoginMsg Login.Msg
    | GotProfileMsg Profile.Msg
    | GotHomeMsg Home.Msg
    | GotChatMsg Chat.Msg
    | GotVerificationMsg Verification.Msg
    | GotSubscriptionChangeMsg Session
    | GotSubscriptionSocketMsg SocketMessageData
    | GetLogout
    | OpenDropdown
    | ChatNewMessage


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

        ChatPage chatModel ->
            Chat.view chatModel
                |> Html.map GotChatMsg

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
            nav []
                [ h1 [] [ a [ href "/" ] [ text "My elm app" ] ]
                , ul []
                    [ case Jwt.decodeToken decodeTokenData <| fromTokenToString token of
                        Ok resultTokenRecord ->
                            li
                                [ classList
                                    [ ( "active"
                                      , isActive { link = Profile resultTokenRecord.id, page = page }
                                      )
                                    ]
                                ]
                                [ div []
                                    [ if String.isEmpty resultTokenRecord.firstname == False then
                                        div
                                            [ onClick OpenDropdown ]
                                            [ span [] [ text (resultTokenRecord.firstname ++ " ⌄") ]
                                            , div [ style "width" "60px" ]
                                                [ case imageStringToMaybeString resultTokenRecord.profilepicurl of
                                                    Just imageString ->
                                                        img [ src imageString, width 60 ] []

                                                    Nothing ->
                                                        identicon 50 50 resultTokenRecord.firstname
                                                ]
                                            ]

                                      else
                                        div
                                            [ onClick OpenDropdown ]
                                            [ span []
                                                [ text
                                                    (resultTokenRecord.email ++ " ⌄")
                                                ]
                                            , div [ style "width" "60px" ]
                                                [ case imageStringToMaybeString resultTokenRecord.profilepicurl of
                                                    Just imageString ->
                                                        img [ src imageString, width 60 ] []

                                                    Nothing ->
                                                        identicon 50 50 resultTokenRecord.email
                                                ]
                                            ]
                                    , ul
                                        [ style "display"
                                            (if openDropdown then
                                                "block"

                                             else
                                                "none"
                                            )
                                        ]
                                        [ li [ onClick OpenDropdown ] [ a [ href <| "/profile/" ++ userIdToString resultTokenRecord.id ] [ text "My profile" ] ]
                                        , li [ onClick OpenDropdown ] [ text "option2" ]
                                        , li [ onClick OpenDropdown ] [ text "option3" ]
                                        ]
                                    ]
                                ]

                        Err err ->
                            li [] [ text (Debug.toString err) ]
                    , li
                        [ classList
                            [ ( "active"
                              , isActive { link = Chat, page = page }
                              )
                            ]
                        ]
                        [ a [ href "/chat" ] [ text "Chat" ] ]
                    , li
                        []
                        [ a [ href "/", onClick GetLogout ] [ text "logout" ] ]
                    ]
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

        ( Chat, ChatPage _ ) ->
            True

        ( Chat, _ ) ->
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

        GotChatMsg chatMsg ->
            case model.page of
                ChatPage chatModel ->
                    let
                        ( chatModelFromChat, chatMsgFromChat ) =
                            Chat.update chatMsg chatModel
                    in
                    ( { model | page = ChatPage chatModelFromChat }, Cmd.map GotChatMsg chatMsgFromChat )

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
                    case Jwt.decodeToken decodeTokenData <| fromTokenToString token of
                        Ok resultTokenRecord ->
                            Nav.pushUrl model.key ("/profile/" ++ userIdToString resultTokenRecord.id)

                        Err _ ->
                            Nav.pushUrl model.key "/login"

                Nothing ->
                    Nav.pushUrl model.key "/login"
            )

        GotSubscriptionSocketMsg socketMsgObj ->
            let
                ( chatModelFromChat, chatMsgFromChat ) =
                    Chat.init model.session <| Just socketMsgObj
            in
            ( { model | page = ChatPage chatModelFromChat }, Cmd.map GotChatMsg chatMsgFromChat )

        -- case model.page of
        --     ChatPage chatModel ->
        --         let
        --             ( chatModelFromChat, chatMsgFromChat ) =
        --                 Chat.update msgToChatMsg chatModel
        --         in
        --         ( { model | page = ChatPage chatModelFromChat }, Cmd.map GotChatMsg chatMsgFromChat )
        --     _ ->
        --         ( model, Cmd.none )
        -- (model, GotSubscriptionSocketMsg)
        -- in
        -- ( model, Cmd.none )
        GetLogout ->
            ( model, logout )

        ChatNewMessage ->
            ( model, Cmd.none )

        OpenDropdown ->
            ( { model | openDropdown = not model.openDropdown }, Cmd.none )


matchRoute : Parser (Route -> a) a
matchRoute =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Profile (s "profile" </> userIdParser)
        , Parser.map Signup (s "signup")
        , Parser.map Chat (s "chat")
        , Parser.map Verification (s "verify-email" </> verifictionStringParser)
        ]


urlToPage : Url -> Session -> Page
urlToPage url session =
    case Parser.parse matchRoute url of
        Just Login ->
            case fromSessionToToken session of
                Just _ ->
                    NotFoundPage

                Nothing ->
                    LoginPage (Tuple.first (Login.init ()))

        Just Signup ->
            case fromSessionToToken session of
                Just _ ->
                    NotFoundPage

                Nothing ->
                    SignupPage (Tuple.first (Signup.init ()))

        Just (Profile _) ->
            case fromSessionToToken session of
                Just _ ->
                    ProfilePage (Tuple.first (Profile.init session))

                Nothing ->
                    NotFoundPage

        Just (Verification _) ->
            case fromSessionToToken session of
                Just _ ->
                    VerificationPage (Tuple.first (Verification.init session url.path))

                Nothing ->
                    NotFoundPage

        Just Chat ->
            case fromSessionToToken session of
                Just _ ->
                    ChatPage (Tuple.first (Chat.init session Nothing))

                Nothing ->
                    NotFoundPage

        Just Home ->
            HomePage (Tuple.first (Home.init ()))

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

        ChatPage _ ->
            let
                ( pageModel, pageCmds ) =
                    Chat.init model.session Nothing
            in
            ( { model | page = ChatPage pageModel }, Cmd.map GotChatMsg pageCmds )

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
    Sub.batch
        [ subscriptionChanges GotSubscriptionChangeMsg model.key
        , socketMessageChanges GotSubscriptionSocketMsg model.key
        ]


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
