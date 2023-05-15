module Home exposing (..)

import Css
import Css.Global
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw


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
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_20 ] ]
        [ Html.h2 []
            [ text "Hello and welcome to our awesome website !" ]
        , Html.p
            []
            [ text "which is still under construction" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
