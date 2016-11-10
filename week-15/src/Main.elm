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


toSvgLine : Line -> Svg.Svg msg
toSvgLine l =
    let
        to =
            { x = 250, y = 250 }

        start =
            transform to (fst l)

        end =
            transform to (snd l)
    in
        Svg.line [ fill "none", stroke "black", x1 (toString start.x), x2 (toString end.x), y1 (toString start.y), y2 (toString end.y) ] []


lines : List Line
lines =
    Turtle.lines [ Forward 10, Right 90, Forward 10, Right 90, Forward 10, Right 90, Forward 10, Right 90 ]
