module Exercise exposing (..)

import BinaryTree exposing (Tree(..), fold, fromList, showTree, sum)
import Html exposing (Html, div, h3, li, p, text, ul)
import Http
import Json.Decode as Decode exposing (Decoder, Value, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, encode)
import List exposing (length)
import RemoteData exposing (WebData)


type alias Model =
    { game : WebData Game
    }


type Game
    = Game
        { homeTeam : String
        , awayTeam : String
        , score : String
        , rootGame : List Game
        }


viewActualGame : Game -> Html Msg
viewActualGame (Game game) =
    let
        viewSubGame : Game -> Html Msg
        viewSubGame subgame =
            viewActualGame subgame

        contents =
            List.map viewSubGame game.rootGame
    in
    ul []
        [ li []
            [ p [] [ text "Home Team:" ]
            , h3 [] [ text game.homeTeam ]
            ]
        , li []
            [ p [] [ text "Away Team:" ]
            , h3 [] [ text game.awayTeam ]
            ]
        , li []
            [ p [] [ text "Score:" ]
            , h3 [] [ text game.score ]
            ]
        , ul [] contents
        ]


type Msg
    = GetShit (WebData Game)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetShit newShit ->
            ( { model | game = newShit }, Cmd.none )


length : List a -> Int
length list =
    lengthHelper list 0


lengthHelper : List a -> Int -> Int
lengthHelper list lengthSoFar =
    case list of
        [] ->
            0

        _ :: rest ->
            lengthHelper rest lengthSoFar + 1


sum : List Int -> Int
sum list =
    sumHelper list 0


sumHelper : List Int -> Int -> Int
sumHelper list sumSoFar =
    case list of
        [] ->
            0

        first :: rest ->
            sumHelper rest first + sumSoFar


exampleTree : Tree Int
exampleTree =
    Node 4 (Node 1 Empty Empty) (Node 9 Empty Empty)


gameFromJson : String -> String -> String -> List Game -> Game
gameFromJson homeTeam awayTeam score rootGames =
    Game
        { homeTeam = homeTeam
        , awayTeam = awayTeam
        , score = score
        , rootGame = rootGames
        }


gameDecoder : Decoder Game
gameDecoder =
    Decode.succeed gameFromJson
        |> required "homeTeam" string
        |> required "awayTeam" string
        |> required "score" string
        |> required "rootGame" (Decode.lazy (\_ -> list gameDecoder))


tournament : Tree String
tournament =
    Node "Winner"
        (Node "1/2"
            (Node "1/4"
                (Node "1/8"
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                )
                (Node "1/8"
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                )
            )
            (Node "1/4"
                (Node "1/8"
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                )
                (Node "1/8"
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                )
            )
        )
        (Node "1/2"
            (Node "1/4"
                (Node "1/8"
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                )
                (Node "1/8"
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                )
            )
            (Node "1/4"
                (Node "1/8"
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                )
                (Node "1/8"
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                    (Node "1/16"
                        (Node "1/32" Empty Empty)
                        (Node "1/32" Empty Empty)
                    )
                )
            )
        )


flatten : Tree a -> List a
flatten tree =
    case tree of
        Empty ->
            []

        Node value left right ->
            flatten left ++ [ value ] ++ flatten right


fromList : List comparable -> Tree comparable
fromList =
    List.foldl insert Empty


member : comparable -> Tree comparable -> Bool
member target tree =
    case tree of
        Empty ->
            False

        Node value left right ->
            if target < value then
                member target left

            else if target > value then
                member target right

            else
                True


insert : comparable -> Tree comparable -> Tree comparable
insert target tree =
    case tree of
        Empty ->
            Node target Empty Empty

        Node value left right ->
            if target < value then
                Node value (insert target left) right

            else if target > value then
                Node value left (insert target right)

            else
                tree


viewGame : WebData Game -> Html Msg
viewGame game =
    case game of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            p [] [ text "Loading some shit... Might take time" ]

        RemoteData.Success newGame ->
            viewActualGame newGame

        RemoteData.Failure err ->
            text "Error!"


viewTournament : Tree String -> Html Msg
viewTournament tree =
    fold
        (\v l r ->
            case tree of
                Empty ->
                    div [] [ text "" ]

                Node value leftTree rightTree ->
                    ul [] [ li [] [ text value, viewTournament rightTree, viewTournament leftTree ] ]
        )
        (\d -> div [] [ text d ])
        tree
        ""


getShit : Cmd Msg
getShit =
    Http.get
        { url = "/.netlify/functions/tournament-api"
        , expect =
            gameDecoder
                |> Http.expectJson (RemoteData.fromResult >> GetShit)
        }


view : Model -> Html Msg
view model =
    div []
        [ p []
            [ length [ 1, 2, 3, 4 ]
                |> String.fromInt
                |> text
            ]
        , p []
            [ sum [ 2, 2, 3 ]
                |> String.fromInt
                |> text
            ]
        , p []
            [ text (Debug.toString (fromList [ 1, 2, 3, 4, 5, 6, 7 ])) ]
        , div []
            [ showTree (fromList [ "1", "1/2", "1/2", "1/4", "1/4", "1/4", "1/4" ]) ]
        , div []
            [ showTree tournament ]
        ]
