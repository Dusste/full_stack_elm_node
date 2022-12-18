module Home exposing (..)

import BinaryTree exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as Decode exposing (Decoder, Value, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, encode)
import RemoteData exposing (WebData)


type alias Model =
    {}


type CheckErrors
    = BadInput String
    | BadRequest String


type alias Data =
    { firstName : String
    , lastName : String
    , isVerified : Bool
    , isAdmin : Bool
    }


initialModel : Model
initialModel =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


type Msg
    = Something


view : Model -> Html Msg
view model =
    div
        []
        [ h2 []
            [ text "Hello and welcome to our awesome website !" ]
        , p
            []
            [ text "which is still under construction" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Something ->
            ( model, Cmd.none )
