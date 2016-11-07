module Main exposing (main)

import Html exposing (..)
import Html.App exposing (program)
import Turtle exposing (..)


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    {}


type Msg
    = Test


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text lines ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


lines : String
lines =
    toString (Turtle.fold [ Forward 10, Right 90, Forward 10, Right 90, Forward 10, Right 90, Forward 10, Right 90 ])
