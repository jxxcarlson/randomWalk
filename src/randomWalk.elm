module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (svg, circle)
import Svg.Attributes as SA exposing (cx, cy, fill, width, height, r)
import Graph exposing (render, prepareIntegerSeries, scaleSeries, flipSeries)


-- cx, cy, fill, width, height

import Html.Attributes exposing (class, id)
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type GameState
    = Running
    | GameOver



-- MODEL


type alias Model =
    { gameState : GameState
    , count : Int
    , register : Int
    , initialBalance : Int
    , balance : Int
    , history : List Int
    , message : String
    }


init : ( Model, Cmd Msg )
init =
    let
        b =
            4
    in
        ( Model Running 0 1 b b [ b ] "Good luck!", Cmd.none )



-- UPDATE


type Msg
    = Roll
    | NewFace Int


updateModel : Int -> Model -> Model
updateModel n model =
    let
        delta =
            if (n % 2 == 0) && model.gameState == Running then
                1
            else if model.gameState == Running then
                -1
            else
                0

        newBalance =
            if model.gameState == Running then
                model.balance + delta
            else
                model.balance

        newGameState =
            if newBalance > 0 then
                Running
            else
                GameOver

        newCount =
            if newGameState == Running then
                model.count + 1
            else
                model.count

        newMessage =
            if newGameState == Running then
                if newBalance < 3 then
                    "Careful!"
                else if newBalance > model.initialBalance then
                    "You are doing well!"
                else
                    "Keep at it!"
            else
                "Game Over!"

        newHistory =
            if newGameState == Running then
                newBalance :: model.history
            else
                model.history
    in
        { model
            | gameState = newGameState
            , count = newCount
            , register = n
            , balance = newBalance
            , history = newHistory
            , message = newMessage
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.int 1 6) )

        NewFace newFace ->
            ( updateModel newFace model, Cmd.none )


dieClass : Model -> Attribute Msg
dieClass model =
    if model.register % 2 == 0 then
        class "green"
    else
        class "red"


balanceClass : Model -> Attribute Msg
balanceClass model =
    if model.balance >= model.initialBalance then
        class "green"
    else if model.balance < model.initialBalance && model.balance > 0 then
        class "yellow"
    else
        class "red"


messageClass : Model -> Attribute Msg
messageClass model =
    if model.balance == 0 then
        class "red"
    else
        class "white"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


dataSet =
    [ ( 20, 100 ), ( 40, 60 ), ( 70, 80 ), ( 100, 20 ) ]


transformData kx ky offset color data =
    data
        |> Graph.prepareIntegerSeries
        |> (Graph.flipSeries offset)
        |> (Graph.scaleSeries kx ky)
        |> Graph.render color


graph : Model -> String -> Svg.Svg msg
graph model color =
    model.history
        |> List.reverse
        |> transformData 3 4 30 color



-- |> Graph.prepareIntegerSeries
-- |> (Graph.flipSeries 30)
-- |> (Graph.scaleSeries 3 4)
-- |> Graph.render color


abscissa : Int -> String -> Svg.Svg msg
abscissa b color =
    List.repeat b 0
        |> transformData 3 4 29.5 color


view : Model -> Html Msg
view model =
    div []
        [ div [ balanceClass model ] [ text ("Bal " ++ (toString model.balance)) ]
        , div [ dieClass model ] [ text ("Die " ++ (toString model.register)) ]
        , div [ class "display" ] [ text ("Count " ++ (toString model.count)) ]
        , button [ onClick Roll ] [ text "Roll" ]
        , br [] []
        , br [] []
        , div [ messageClass model ] [ text model.message ]
        , svg
            [ SA.width "1200", SA.height "400" ]
            [ (graph model "yellow")
            , (abscissa 300 "white")
            ]
        ]
