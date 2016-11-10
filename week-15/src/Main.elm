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
    = Msg


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg [ viewBox "0 0 1000 1000", Svg.Attributes.width "1000px" ]
            (List.map
                toSvgLine
                (lines actions)
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


transform : Point -> Point -> Point
transform to from =
    let
        rotated =
            rotate 180 from
    in
        { x = rotated.x + to.x, y = rotated.y + to.y }


rotate : Int -> Point -> Point
rotate angle point =
    let
        rad =
            degrees (toFloat angle)

        x =
            toFloat point.x

        y =
            toFloat point.y

        newX =
            round (x * (cos rad) - y * (sin rad))

        newY =
            round (y * (cos rad) + x * (sin rad))
    in
        { x = newX, y = newY }


toSvgLine : Turtle.Line -> Svg.Svg msg
toSvgLine l =
    let
        to =
            { x = 500, y = 500 }

        start =
            transform to l.start

        end =
            transform to l.end
    in
        Svg.line
            [ fill "none"
            , stroke l.color
            , strokeWidth (toString l.width)
            , x1 (toString start.x)
            , x2 (toString end.x)
            , y1 (toString start.y)
            , y2 (toString end.y)
            ]
            []


lines : List Action -> List Turtle.Line
lines actions =
    Turtle.lines
        (List.concat
            [ [ Push ]
            , actions
            , [ Pop, Scale 3, PenUp, Left 90, Forward 50, Right 90, PenDown ]
            , actions
            ]
        )


actions : List Action
actions =
    [ Width 1
    , Forward 30
    , Right 90
    , Color "red"
    , Push
    , Width 2
    , Forward 30
    , Right 90
    , Color "blue"
    , Width 3
    , Forward 30
    , Right 90
    , Color "green"
    , Width 4
    , Forward 30
    , Pop
    , PenUp
    , Left 90
    , Forward 30
    , PenDown
    , Right 45
    , Forward 30
    ]
