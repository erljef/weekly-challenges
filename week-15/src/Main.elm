module Main exposing (main)

import Array exposing (..)
import Html exposing (..)
import Html.App exposing (program)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Turtle exposing (..)


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Line l =
    { l
        | start : Point
        , end : Point
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
    div []
        [ Svg.svg [ viewBox "0 0 500 500", Svg.Attributes.width "500px" ]
            (List.map
                toSvgLine
                lines
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


transform : Point -> Point -> Point
transform to from =
    { x = from.x + to.x, y = from.y + to.y }


toSvgLine : Turtle.Line -> Svg.Svg msg
toSvgLine l =
    let
        to =
            { x = 250, y = 250 }

        start =
            transform to l.start

        end =
            transform to l.end
    in
        Svg.line [ fill "none", stroke l.color, x1 (toString start.x), x2 (toString end.x), y1 (toString start.y), y2 (toString end.y) ] []


lines : List Turtle.Line
lines =
    Turtle.lines [ Forward 10, Right 90, Color "red", Forward 10, Right 90, Color "blue", Forward 10, Right 90, Color "green", Forward 10, Right 90 ]
