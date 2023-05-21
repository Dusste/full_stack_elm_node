module Profile exposing (..)

import Credentials
    exposing
        ( ImageString
        , Session
        , Token
        , UnwrappedTokenData
        , addHeader
        , decodeTokenData
        , emptyImageString
        , emptyUserId
        , emptyVerificationString
        , encodeImageString
        , encodeToken
        , fromSessionToToken
        , fromTokenToString
        , imageStringToMaybeString
        , logout
        , storeSession
        , stringToImageString
        , tokenDecoder
        )
import Css
import Css.Global
import File exposing (File)
import File.Select as Select
import GlobalStyles as Gs
import Helpers exposing (buildErrorMessage)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr exposing (src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode exposing (encode)
import Jwt
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task


type alias Model =
    { profile : UnwrappedTokenData
    , errors : List CheckErrors
    , imageFile : ImageString
    , userState : UserState
    }


type alias ProfileSubmitData =
    { email : String
    , firstname : String
    , imagefile : ImageString
    }


type CheckErrors
    = BadInput String
    | BadRequest String


type UserState
    = NotVerified
    | Verified Session
    | Intruder
    | SessionExpired


type Msg
    = StoreFirstName String
      -- | StoreLastName String
    | StoreVerified Bool
      -- | StoreAdmin Bool
    | ProfileSubmit Session ProfileSubmitData
    | ProfileDone (Result Http.Error Token)
    | FileRequest
    | FileRequestProceed File
    | FileRead (Result Http.Error String)
    | LogoutUser


profileSubmitDataEncoder : ProfileSubmitData -> Encode.Value
profileSubmitDataEncoder profileData =
    Encode.object
        [ ( "email", Encode.string profileData.email )
        , ( "firstname", Encode.string profileData.firstname )
        , ( "imagefile", encodeImageString profileData.imagefile )
        ]


initialModel : Model
initialModel =
    { profile =
        { firstname = ""
        , email = ""
        , id = emptyUserId
        , isverified = False
        , verificationstring = emptyVerificationString
        , profilepicurl = emptyImageString
        }
    , errors = []
    , imageFile = emptyImageString
    , userState = NotVerified
    }


init : Session -> ( Model, Cmd Msg )
init session =
    case fromSessionToToken session of
        Just token ->
            case Jwt.decodeToken decodeTokenData <| fromTokenToString token of
                Ok profileData ->
                    ( { initialModel
                        | profile = profileData
                        , userState =
                            if profileData.isverified then
                                Verified session

                            else
                                NotVerified
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { initialModel | userState = Intruder }
                    , Cmd.none
                    )

        Nothing ->
            ( { initialModel | userState = Intruder }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.userState of
        Verified session ->
            Html.div
                [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ] ]
                [ Html.h2 [ Attr.css [ Tw.text_3xl ] ] [ text "Hello" ]
                , Html.form [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ] ]
                    [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                        [ text "First Name"
                        , Html.input
                            [ Attr.css Gs.inputStyle
                            , type_ "text"
                            , onInput StoreFirstName
                            , value model.profile.firstname
                            ]
                            []
                        ]

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
                    , Html.div [ Attr.css [ Tw.flex, Tw.gap_3 ] ]
                        [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col ] ]
                            [ Html.p [ Attr.css [ Tw.m_0 ] ] [ text "Upload an avatar" ]
                            , Html.p [ Attr.css [ Tw.m_0, Tw.text_sm, Tw.text_color Tw.gray_400 ] ] [ text "(Size limit is 3 mb)" ]
                            ]
                        , Html.label [ Attr.for "file", Attr.css <| Gs.buttonStyle ++ [ Tw.overflow_hidden ] ]
                            [ text "Choose file"
                            , Html.input
                                [ Attr.css [ Tw.w_1, Tw.h_1, Tw.overflow_hidden, Tw.opacity_0, Tw.absolute, Tw.z_0 ]
                                , Attr.id "file"
                                , type_ "file"
                                , onClick FileRequest
                                ]
                                []
                            ]
                        ]
                    , case imageStringToMaybeString model.imageFile of
                        Just imageString ->
                            Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                                [ text "Your avatar preview"
                                , Html.img
                                    [ Attr.css [ Tw.rounded ], src imageString ]
                                    []
                                ]

                        Nothing ->
                            text ""

                    -- , div []
                    --     [ text "Admin"
                    --     , br [] []
                    --     , input
                    --         [ type_ "checkbox"
                    --         , onClick (StoreAdmin (not model.profile.isadmin))
                    --         ]
                    --         []
                    --     ]
                    , Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                        [ let
                            { firstname, email } =
                                model.profile

                            { imageFile } =
                                model
                          in
                          Html.button
                            [ Attr.css Gs.buttonStyle
                            , type_ "button"
                            , onClick (ProfileSubmit session { firstname = firstname, email = email, imagefile = imageFile })
                            ]
                            [ text "Submit" ]
                        ]
                    , Html.ul []
                        (List.map viewError model.errors)
                    ]
                ]

        NotVerified ->
            Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ] ]
                [ Html.h2 [] [ text "Please verify your email ! " ]
                , Html.p []
                    [ text "You can't access your profile until you verify your email" ]
                ]

        Intruder ->
            Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ] ]
                [ Html.h2 [] [ text "Hmm seems you are not logged in" ]
                , Html.p []
                    [ text "Please create account or login" ]
                ]

        SessionExpired ->
            Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ] ]
                [ Html.h2 [] [ text "Your session have expired" ]
                , Html.p []
                    [ text "Please login again" ]
                ]


viewError : CheckErrors -> Html Msg
viewError checkErrors =
    Html.li []
        [ Html.p []
            [ text
                (case checkErrors of
                    BadInput err ->
                        err

                    BadRequest err ->
                        err
                )
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
            let
                validateFirstName =
                    cred.firstname
                        |> String.trim
                        |> String.split " "
                        |> String.join ""
            in
            ( model, submitProfile session { cred | firstname = validateFirstName } )

        ProfileDone (Ok token) ->
            let
                tokenValue =
                    encodeToken token
            in
            ( model, storeSession <| Just <| encode 0 tokenValue )

        ProfileDone (Err error) ->
            ( { model | errors = [ BadRequest "Something went wrong !" ] }
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
            ( model, Select.file [ "image/*" ] FileRequestProceed )

        FileRequestProceed file ->
            ( model, Task.attempt FileRead (File.toUrl file) )

        FileRead (Ok imageFileString) ->
            let
                trimImageString =
                    String.trim imageFileString

                oldProfile =
                    model.profile

                imageString =
                    stringToImageString trimImageString

                updateProfile =
                    { oldProfile | profilepicurl = imageString }
            in
            ( { model | profile = updateProfile, imageFile = imageString }, Cmd.none )

        FileRead (Err error) ->
            ( { model | errors = BadRequest (buildErrorMessage error) :: model.errors }, Cmd.none )

        LogoutUser ->
            ( model, logout )


submitProfile : Session -> ProfileSubmitData -> Cmd Msg
submitProfile session data =
    case fromSessionToToken session of
        Just token ->
            Http.request
                { method = "PUT"
                , headers = [ addHeader token ]
                , url = "/api/profile"
                , body = Http.jsonBody (profileSubmitDataEncoder data)
                , expect = Http.expectJson ProfileDone tokenDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none
