module TrucoSpec exposing (suite)

import Main exposing (Msg(..), view)
import Test exposing (Test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Html
import Expect


suite : Test
suite =
    Test.describe "Main"
        [ Test.test "Displays the players names" <|
            \_ ->
                view ({ name = "Elm", points = 0 }, { name = "JS", points = 0 })
                    |> Query.fromHtml
                    |> Query.has [Html.text "Elm"]
        , Test.test "Displays all the controls" <|
            \_ ->
                view ({ name = "Elm", points = 0 }, { name = "JS", points = 0 })
                    |> Query.fromHtml
                    |> Query.findAll [ Html.tag "button" ]
                    |> Query.count (Expect.equal 4)
        , Test.test "Increments playerOne score" <|
            \_ ->
                view ({ name = "Elm", points = 0 }, { name = "JS", points = 0 })
                    |> Query.fromHtml
                    |> Query.findAll [ Html.tag "button" ]
                    |> Query.index 0
                    |> Event.simulate Event.click
                    |> Event.expect IncrementScorePlayerOne
        , Test.test "Decrements playerOne score" <|
            \_ ->
                view ({ name = "Elm", points = 0 }, { name = "JS", points = 0 })
                    |> Query.fromHtml
                    |> Query.findAll [ Html.tag "button" ]
                    |> Query.index 1
                    |> Event.simulate Event.click
                    |> Event.expect DecrementScorePlayerOne
        , Test.test "Increments playerTwo score" <|
            \_ ->
                view ({ name = "Elm", points = 0 }, { name = "JS", points = 0 })
                    |> Query.fromHtml
                    |> Query.findAll [ Html.tag "button" ]
                    |> Query.index 2
                    |> Event.simulate Event.click
                    |> Event.expect IncrementScorePlayerTwo
        , Test.test "Decrements playerTwo score" <|
            \_ ->
                view ({ name = "Elm", points = 0 }, { name = "JS", points = 0 })
                    |> Query.fromHtml
                    |> Query.findAll [ Html.tag "button" ]
                    |> Query.index 3
                    |> Event.simulate Event.click
                    |> Event.expect DecrementScorePlayerTwo
        ]
