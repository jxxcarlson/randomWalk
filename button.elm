module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)


type alias Model =
    { register : Int
    }


initialModel =
    { register = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | register = model.register + 1 }, Cmd.none )

        Decrement ->
            ( { model | register = model.register - 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [ Html.Attributes.id "register" ]
            [ text (toString model.register) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Decrement ] [ text "-" ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
