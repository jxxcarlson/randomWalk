module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as HA exposing (class, id, style)
import Time exposing (Time, second)
import Svg exposing (svg, circle)
import Svg.Attributes as SA exposing (..)
import Random
import Graph
    exposing
        ( drawPointList
        , drawIntegerTimeSeries
        , drawLine
        )


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


type RunMode
    = Manual
    | Automatic



-- MODEL


type alias Model =
    { gameState : GameState
    , runMode : RunMode
    , count : Int
    , dieFace : Int
    , initialBalance : Int
    , balance : Int
    , history : List Int
    , message : String
    , info : String
    , graphData : Graph.GraphData
    }


init : ( Model, Cmd Msg )
init =
    let
        initialBalance =
            4

        source =
            Graph.Rect 0.0 0.0 200.0 20.0

        target =
            Graph.Rect 0.0 0.0 800.0 120.0

        graphData =
            Graph.GraphData source target "black" "white"
    in
        ( Model Running
            Manual
            0
            1
            initialBalance
            initialBalance
            [ initialBalance ]
            "Good luck!"
            "Simulator on!"
            graphData
        , Cmd.none
        )



-- UPDATE


type Msg
    = Roll
    | NewFace Int
    | Reset
    | Run
    | Tick Time


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

        newHistory =
            if model.gameState == Running then
                newBalance :: model.history
            else
                model.history

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
    in
        { model
            | gameState = newGameState
            , count = newCount
            , dieFace = n
            , balance = newBalance
            , history = newHistory
            , info = newHistory |> List.take 40 |> List.reverse |> intList2String
            , message = newMessage
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.int 1 6) )

        NewFace newFace ->
            ( updateModel newFace model, Cmd.none )

        Reset ->
            init

        Run ->
            ( { model | runMode = Automatic }, Cmd.none )

        Tick newTime ->
            if model.runMode == Automatic && model.gameState == Running then
                ( model, Random.generate NewFace (Random.int 1 6) )
            else
                ( model, Cmd.none )


{-| The next three functions change the color of text elements
in accord with the state of the Game
-}
dieClass : Model -> Attribute Msg
dieClass model =
    if model.dieFace % 2 == 0 then
        SA.class "green"
    else
        SA.class "red"


balanceClass : Model -> Attribute Msg
balanceClass model =
    if model.balance >= model.initialBalance then
        SA.class "green"
    else if model.balance < model.initialBalance && model.balance > 0 then
        SA.class "yellow"
    else
        SA.class "red"


defaultMessageStyle =
    [ ( "width", "200px" ), ( "padding", "8px" ) ]


messageClass : Model -> Attribute Msg
messageClass model =
    if model.balance == 0 then
        HA.style ([ ( "background-color", "red" ), ( "color", "white" ) ] ++ defaultMessageStyle)
    else
        HA.style ([ ( "background-color", "green" ), ( "color", "white" ) ] ++ defaultMessageStyle)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (0.2 * second) Tick



-- VIEW


{-| The main graphing function:displays graph of the current game history
-}
graph : Model -> String -> Svg.Svg msg
graph model color =
    model.history
        |> List.reverse
        |> Graph.drawIntegerTimeSeries model.graphData color


{-| intList2String [1, 4, 2, 7] = "1, 4, 2, 7"
-}
intList2String : List Int -> String
intList2String list =
    List.map toString list |> String.join (", ")


view : Model -> Html Msg
view model =
    div
        [ HA.style
            [ ( "margin-left", "35px" )
            , ( "margin-top", "35px" )
            , ( "padding", "45px 45px 45px 45px" )
            , ( "width", "820px" )
            , ( "height", "350px" )
            , ( "background-color", "#eee" )
            ]
        ]
        [ div [ labelStyle ] [ text ("Bal " ++ (toString model.balance)) ]
        , div [ labelStyle ] [ text ("Die " ++ (toString model.dieFace)) ]
        , div [ SA.class "display", labelStyle ] [ text ("Count " ++ (toString model.count)) ]
        , button [ onClick Roll, buttonStyle ] [ text "Roll" ]
        , button [ onClick Reset, SA.id "reset", buttonStyle ] [ text "Reset" ]
        , button [ onClick Run, SA.id "run", buttonStyle ] [ text "Run" ]
        , br [] []
        , br [] []
        , div [ messageClass model ] [ text model.message ]
        , p [ SA.id "info" ] [ text model.info ]
        , svg
            [ SA.width "1200", SA.height "400" ]
            [ (graph model "yellow")
            , (Graph.boundingRect model.graphData)
            , (Graph.drawLine model.graphData
                "red"
                0.0
                (toFloat model.initialBalance)
                200.0
                (toFloat model.initialBalance)
              )
            , (graph model "yellow")
            ]
        ]


buttonStyle =
    HA.style
        [ ( "height", "25px" )
        , ( "background-color", "black" )
        , ( "color", "white" )
        , ( "margin-right", "10px" )
        , ( "font-size", "12pt" )
        ]


labelStyle =
    HA.style
        [ ( "height", "20px" )
        , ( "width", "55px" )
        , ( "background-color", "black" )
        , ( "color", "white" )
        , ( "margin-bottom", "10px" )
        , ( "font-size", "10pt" )
        , ( "padding", "5px 8px 0px 8px" )
        ]
