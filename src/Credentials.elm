port module Credentials exposing
    ( ProfileData
    , Session
    , Token
    , UnwrappedTokenData
    , UserId
    , VerificationString
    , addHeader
    , decodeToSession
    , decodeTokenData
    , emptyUserId
    , emptyVerificationString
    , encodeToken
    , fromSessionToToken
    , fromTokenToString
    , guest
    , isSessionValid
    , logout
    , onSessionChange
    , sessionToVerificationString
    , storeSession
    , subscriptionChanges
    , tokenDecoder
    , unfoldProfileFromToken
    , unwrappedTokenDataEncoder
    , userIdParser
    , userIdToString
    , verificationToString
    , verifictionStringParser
    )

import Array
import Base64 exposing (decode)
import Browser.Navigation as Nav
import Http exposing (Header, header)
import Json.Decode as Decode exposing (Decoder, Value, at, bool, map6, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Url.Parser exposing (Parser, custom)


type Token
    = Token String


type Session
    = LoggedIn Token
    | Guest


type VerificationString
    = VerificationString String


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



-- TODO need some vaildation ?


verificationToString : VerificationString -> String
verificationToString (VerificationString verificationString) =
    verificationString


emptyUserId : UserId
emptyUserId =
    UserId ""


emptyVerificationString : VerificationString
emptyVerificationString =
    VerificationString ""


guest : Session
guest =
    Guest


userIdParser : Parser (UserId -> a) a
userIdParser =
    custom "USERID" <|
        \userId ->
            Maybe.map UserId (Just userId)


verifictionStringParser : Parser (VerificationString -> a) a
verifictionStringParser =
    custom "VERIFICATIONSTRING" <|
        \verificationString ->
            Maybe.map VerificationString (Just verificationString)


encodeUserId : UserId -> Encode.Value
encodeUserId (UserId id) =
    Encode.string id


idDecoder : Decoder UserId
idDecoder =
    Decode.map UserId string


verifyStringDecoder : Decoder VerificationString
verifyStringDecoder =
    Decode.map VerificationString string


type alias UnwrappedTokenData =
    { id : UserId
    , isverified : Bool
    , email : String
    , firstname : String
    , verificationstring : VerificationString
    , profilepicurl : String
    }


fromSessionToToken : Session -> Maybe Token
fromSessionToToken session =
    case session of
        LoggedIn token ->
            Just token

        Guest ->
            Nothing


isSessionValid : Session -> Bool
isSessionValid session =
    case session of
        LoggedIn _ ->
            True

        Guest ->
            False



-- skontaj kako da validirsas da je string token
-- fromStringToToken : String -> Token
-- fromStringToToken string =
--     Token string


fromTokenToString : Token -> String
fromTokenToString (Token string) =
    string


unwrappedTokenDataEncoder : UnwrappedTokenData -> Encode.Value
unwrappedTokenDataEncoder profileData =
    Encode.object
        [ ( "id", encodeUserId profileData.id )
        , ( "firstname", Encode.string profileData.firstname )
        , ( "email", Encode.string profileData.email )
        , ( "isverified", Encode.bool profileData.isverified )
        , ( "profilepicurl", Encode.string profileData.profilepicurl )
        ]


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


sessionToVerificationString : Session -> VerificationString
sessionToVerificationString session =
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
                            emptyVerificationString

                        Ok encodedRecord ->
                            case Decode.decodeString profileFromToken encodedRecord of
                                Ok resultTokenRecord ->
                                    resultTokenRecord.verificationstring

                                Err _ ->
                                    emptyVerificationString

                Nothing ->
                    emptyVerificationString

        Nothing ->
            emptyVerificationString


decodeTokenData : Decoder UnwrappedTokenData
decodeTokenData =
    map6 UnwrappedTokenData
        (at [ "id" ] idDecoder)
        (at [ "isverified" ] bool)
        (at [ "email" ] string)
        (at [ "firstname" ] string)
        (at [ "verificationstring" ] verifyStringDecoder)
        (at [ "profilepicurl" ] string)


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


addHeader : Token -> Header
addHeader (Token tokenString) =
    header "authorization" ("Token " ++ tokenString)
