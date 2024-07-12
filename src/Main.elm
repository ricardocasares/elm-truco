module Main exposing (Model, Msg(..), Player, main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr


type alias Player =
    { name : String
    , points : Int
    }


type alias Model =
    ( Player, Player )


type Msg
    = IncrementScorePlayerOne
    | IncrementScorePlayerTwo
    | DecrementScorePlayerOne
    | DecrementScorePlayerTwo
    | SetNamePlayerOne String
    | SetNamePlayerTwo String


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    ( { name = "Nosotros", points = 0 }, { name = "Ellos", points = 0 } )


update : Msg -> Model -> Model
update msg ( a, b ) =
    case msg of
        SetNamePlayerOne name ->
            ( Player name a.points, b )

        SetNamePlayerTwo name ->
            ( a, Player name b.points )

        IncrementScorePlayerOne ->
            ( Player a.name (notMoreThanThirty a.points), b )

        IncrementScorePlayerTwo ->
            ( a, Player b.name (notMoreThanThirty b.points) )

        DecrementScorePlayerOne ->
            ( Player a.name (notLessThanZero a.points), b )

        DecrementScorePlayerTwo ->
            ( a, Player b.name (notLessThanZero b.points) )


doWhen : (a -> a) -> Bool -> a -> a
doWhen f a b =
    if a then
        f b

    else
        b


notLessThanZero : Int -> Int
notLessThanZero a =
    doWhen (\x -> x - 1) (a > 0) a


notMoreThanThirty : Int -> Int
notMoreThanThirty a =
    doWhen (\x -> x + 1) (a < 30) a


view : Model -> Html Msg
view ( playerOne, playerTwo ) =
    div [ class "flex flex-col h-full" ]
        [ div [ class "flex h-full" ]
            [ div [ class "w-full flex flex-col border-r border-neutral" ]
                [ scoreDisplay playerOne SetNamePlayerOne
                , scoreBoard playerOne
                , scoreControls IncrementScorePlayerOne DecrementScorePlayerOne
                ]
            , div [ class "w-full flex flex-col" ]
                [ scoreDisplay playerTwo SetNamePlayerTwo
                , scoreBoard playerTwo
                , scoreControls IncrementScorePlayerTwo DecrementScorePlayerTwo
                ]
            ]
        ]


scoreDisplay : Player -> (String -> Msg) -> Html Msg
scoreDisplay { name, points } msg =
    div [ class "flex p-2 gap-2", stageIndicator points ]
        [ input [ class "bg-transparent text-center font-mono font-bold grow appearance-none w-full", onInput msg, value name ] [ text name ]
        , div [ class "font-bold font-mono tabular-nums" ] [ text (String.fromInt points) ]
        ]


scoreBoard : Player -> Html msg
scoreBoard { points } =
    div [ class "flex flex-col h-full items-center mt-8 gap-10" ]
        [ if points < 6 || points > 15 && points < 21 then
            div [ scoreBox points ] []

          else
            div [ scoreBox 5 ] []
        , if points < 6 || points > 15 && points < 21 then
            div [] []

          else if points < 11 || points > 20 && points < 26 then
            div [ scoreBox points ] []

          else
            div [ scoreBox 5 ] []
        , if points < 11 || points > 15 && points < 26 then
            div [] []

          else if points > 26 || points > 10 && points < 30 then
            div [ scoreBox points ] []

          else
            div [ scoreBox 5 ] []
        ]


scoreBox : Int -> Html.Attribute msg
scoreBox points =
    if points == 0 then
        class ""

    else
        case modBy 5 points of
            1 ->
                class "w-32 h-32 border-accent border-t-4"

            2 ->
                class "w-32 h-32 border-accent border-t-4 border-r-4"

            3 ->
                class "w-32 h-32 border-accent border-t-4 border-r-4 border-b-4"

            4 ->
                class "w-32 h-32 border-accent border-4"

            0 ->
                class "w-32 h-32 border-accent border-4 bg-accent"

            _ ->
                class "bg-red-500"


scoreControls : msg -> msg -> Html msg
scoreControls a b =
    div [ class "flex gap-4 p-4" ]
        [ button [ class "grow btn btn-primary touch-manipulation", onClick a ] [ plusIcon ]
        , button [ class "grow btn btn-secondary touch-manipulation", onClick b ] [ minusIcon ]
        ]


badStage : Html.Attribute msg
badStage =
    class "bg-neutral text-accent"


goodStage : Html.Attribute msg
goodStage =
    class "bg-accent text-neutral"


stageIndicator : Int -> Html.Attribute msg
stageIndicator n =
    if n > 15 then
        goodStage

    else
        badStage


plusIcon : Html msg
plusIcon =
    svg
        [ SvgAttr.width "32"
        , SvgAttr.height "32"
        , SvgAttr.viewBox "0 0 24 24"
        ]
        [ path
            [ SvgAttr.fill "none"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.d "M5 12h14m-7-7v14"
            ]
            []
        ]


minusIcon : Html msg
minusIcon =
    svg
        [ SvgAttr.width "32"
        , SvgAttr.height "32"
        , SvgAttr.viewBox "0 0 24 24"
        ]
        [ path
            [ SvgAttr.fill "none"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.d "M5 12h14"
            ]
            []
        ]
