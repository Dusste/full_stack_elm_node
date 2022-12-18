port module Credentials exposing (ProfileData, Session, Token, UnwrappedTokenData, UserId, addHeader, decodeToSession, decodeTokenData, emptyUserId, encodeToken, fromSessionToToken, fromTokenToString, logout, onSessionChange, profileDataDecoder, profileDataEncoder, storeSession, subscriptionChanges, tokenDecoder, unfoldProfileFromToken, unwrappedTokenDataEncoder, userIdParser, userIdToString)

import Browser.Navigation as Nav
import Http exposing (Header, header)
import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder, Value, at, bool, int, map6, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Url.Parser exposing (Parser, custom)


type Token
    = Token String


type Session
    = LoggedIn Token
    | Guest


type alias ProfileData =
    { firstName : String
    , lastName : String
    , isVerified : Bool
    , isAdmin : Bool
    }


type UserId
    = UserId String


userIdToString : UserId -> String
userIdToString (UserId id) =
    id


emptyUserId : UserId
emptyUserId =
    UserId ""



-- stringToUserId : String -> String
-- stringToUserId (UserId id) =
--     String.fromInt id


userIdParser : Parser (UserId -> a) a
userIdParser =
    custom "USERID" <|
        \userId ->
            Maybe.map UserId (Just userId)


encodeUserId : UserId -> Encode.Value
encodeUserId (UserId id) =
    Encode.string id


idDecoder : Decoder UserId
idDecoder =
    Decode.map UserId string


type alias UnwrappedTokenData =
    { id : UserId
    , isverified : Bool
    , email : String
    , firstname : String
    , iat : Int
    , exp : Int
    }


fromSessionToToken : Session -> Maybe Token
fromSessionToToken session =
    case session of
        LoggedIn token ->
            Just token

        Guest ->
            Nothing



-- skontaj kako da validirsas da je string token
-- fromStringToToken : String -> Token
-- fromStringToToken string =
--     Token string


fromTokenToString : Token -> String
fromTokenToString (Token string) =
    string


profileDataEncoder : ProfileData -> Encode.Value
profileDataEncoder profileData =
    Encode.object
        [ ( "firstName", Encode.string profileData.firstName )
        , ( "lastName", Encode.string profileData.lastName )
        , ( "isVerified", Encode.bool profileData.isVerified )
        , ( "isAdmin", Encode.bool profileData.isAdmin )
        ]


unwrappedTokenDataEncoder : UnwrappedTokenData -> Encode.Value
unwrappedTokenDataEncoder profileData =
    Encode.object
        [ ( "id", encodeUserId profileData.id )
        , ( "firstname", Encode.string profileData.firstname )
        , ( "email", Encode.string profileData.email )
        , ( "isverified", Encode.bool profileData.isverified )
        , ( "iat", Encode.int profileData.iat )
        , ( "exp", Encode.int profileData.exp )
        ]


profileDataDecoder : Decoder ProfileData
profileDataDecoder =
    Decode.succeed ProfileData
        |> required "firstName" string
        |> required "lastName" string
        |> required "isVerified" bool
        |> required "isAdmin" bool


tokenDecoder : Decoder Token
tokenDecoder =
    Decode.succeed Token
        |> required "token" string


encodeToken : Token -> Value
encodeToken (Token token) =
    Encode.object
        [ ( "token", Encode.string token ) ]


port storeSession : Maybe String -> Cmd msg


logout : Cmd msg
logout =
    storeSession Nothing


port onSessionChange : (Value -> msg) -> Sub msg


decodeTokenData : Decoder UnwrappedTokenData
decodeTokenData =
    map6 UnwrappedTokenData
        (at [ "id" ] idDecoder)
        (at [ "isverified" ] bool)
        (at [ "email" ] string)
        (at [ "firstname" ] string)
        (at [ "iat" ] int)
        (at [ "exp" ] int)


unfoldProfileFromToken : Token -> Decoder UnwrappedTokenData
unfoldProfileFromToken (Token tokenData) =
    decodeTokenData


decodeToSession : Nav.Key -> Value -> Session
decodeToSession key value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    case
        Decode.decodeValue Decode.string value
            |> Result.andThen (Decode.decodeString tokenDecoder)
            |> Result.toMaybe
    of
        Just token ->
            LoggedIn token

        Nothing ->
            Guest


subscriptionChanges : (Session -> msg) -> Nav.Key -> Sub msg
subscriptionChanges toMsg key =
    onSessionChange (\val -> toMsg (decodeToSession key val))



-- addHeader : Token -> RequestBuilder a -> RequestBuilder a
-- addHeader (Token tokenString) builder =
--     builder
--         |> withHeader "authorization" ("Token " ++ tokenString)


addHeader : Token -> Header
addHeader (Token tokenString) =
    header "authorization" ("Token " ++ tokenString)
