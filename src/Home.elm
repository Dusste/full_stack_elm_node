module Home exposing (..)

import Html exposing (..)


type alias Model =
    {}


type CheckErrors
    = BadInput String
    | BadRequest String


initialModel : Model
initialModel =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


type Msg
    = NoOp


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
        NoOp ->
            ( model, Cmd.none )
