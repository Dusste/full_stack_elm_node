{- OVERVIEW ------------------------------------------------------
   A "Tree" represents a binary tree. A "Node" in a binary tree
   always has two children. A tree can also be "Empty". Below I have
   defined "Tree" and a number of useful functions.
   This example also includes some challenge problems!
   https://gist.github.com/lovasoa/1f55ad8ec12dd37edb7fce78e04e8974
   ----------------------------------------------------------------
-}


module BinaryTree exposing (Tree(..), count, deepTree, depth, display, flatten, fold, fromList, insert, isElement, main, map, niceTree, reduce, showTree, sum)

import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (style)



-- TREES


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
        Empty ->
            Node x Empty Empty

        Node y left right ->
            if x > y then
                Node y left (insert x right)

            else if x < y then
                Node y (insert x left) right

            else
                tree


fromList : List comparable -> Tree comparable
fromList =
    List.foldl insert Empty


count : Tree a -> number
count =
    fold (\_ n -> n + 1) 0


depth : Tree a -> number
depth =
    reduce (\v l r -> 1 + max l r) 0


sum : Tree number -> number
sum =
    fold (+) 0


flatten : Tree a -> List a
flatten =
    fold (::) []


isElement : a -> Tree a -> Bool
isElement x =
    fold (\el b -> b || el == x) False


map : (a -> b) -> Tree a -> Tree b
map f =
    reduce (\v left right -> Node (f v) left right) Empty


fold : (a -> b -> b) -> b -> Tree a -> b
fold fun init tree =
    case tree of
        Empty ->
            init

        Node v left right ->
            fold fun (fun v (fold fun init left)) right


reduce : (a -> b -> b -> b) -> b -> Tree a -> b
reduce fun init tree =
    case tree of
        Empty ->
            init

        Node v left right ->
            fun v (reduce fun init left) (reduce fun init right)



-- PLAYGROUND


deepTree =
    fromList [ "5", "3", "8", "4", "6", "2", "9", "7", "0" ]


niceTree =
    fromList [ 2, 1, 3 ]


main =
    div [ style "font-family" "monospace" ]
        [ text "deepTree"
        , showTree deepTree
        , display "depth deepTree" (depth deepTree)
        , display "flatten deepTree" (flatten deepTree)
        , display "incremented" (map (\n -> n + 1) niceTree)
        ]



-- toString : a -> String
-- toString value =
--     value


display : String -> a -> Html msg
display name value =
    div [] [ text (name ++ " ==> " ++ Debug.toString value) ]


showTree : Tree String -> Html msg
showTree =
    reduce
        (\v l r ->
            ul
                [ style "border" "1px solid black", style "padding" "3px", style "display" "inline-block" ]
                [ li [ style "text-align" "center" ] [ text <| Debug.toString v ], l, r ]
        )
        (text "")
