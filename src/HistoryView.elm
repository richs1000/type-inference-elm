module HistoryView exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import HistoryTypes exposing (HistoryList)
import MessageTypes exposing (Msg(..))


type alias Pixels =
    Int


historySquareSize : Pixels
historySquareSize =
    25


historySquareSeparation : Pixels
historySquareSeparation =
    5


historySection : HistoryList -> Int -> Html Msg
historySection history historyLength =
    Svg.svg
        [ version "1.1"
        , baseProfile "full"
        , Svg.Attributes.width
            (toString
                (historyLength
                    * (historySquareSize + historySquareSeparation)
                )
            )
        , Svg.Attributes.height "50"
        ]
        (historyList history 0)


historySquare : Maybe Bool -> Int -> Svg a
historySquare h i =
    let
        base =
            [ Svg.Attributes.width (toString historySquareSize)
            , Svg.Attributes.height (toString historySquareSize)
            , y "10"
            ]

        incorrect =
            List.append base [ fill "red" ]

        correct =
            List.append base [ fill "green" ]

        nothing =
            List.append base [ fill "white" ]

        myX =
            10 + (historySquareSize + historySquareSeparation) * i
    in
        case h of
            Just True ->
                rect (List.append [ x (toString myX) ] correct) []

            Just False ->
                rect (List.append [ x (toString myX) ] incorrect) []

            Nothing ->
                rect (List.append [ x (toString myX) ] nothing) []


historyList : HistoryList -> Int -> List (Svg a)
historyList history index =
    case history of
        h :: hs ->
            (historySquare h index) :: (historyList hs (index + 1))

        [] ->
            []
