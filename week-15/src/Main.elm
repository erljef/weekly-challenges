module Main exposing (main)

import Array exposing (..)
import Html exposing (..)
import Html.App exposing (program)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Turtle exposing (..)
import Debug exposing (log)
import Window exposing (..)
import Task exposing (perform)


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { windowHeight : Int
    , windowWidth : Int
    }


type Msg
    = WindowSize Int Int


init : ( Model, Cmd Msg )
init =
    ( { windowHeight = 0, windowWidth = 0 }
    , Task.perform (\_ -> WindowSize 0 0) (\{ height, width } -> WindowSize height width) Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowSize h w ->
            ( { model | windowHeight = h - 50, windowWidth = w - 50 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg [ Svg.Attributes.width (toString model.windowWidth), Svg.Attributes.height (toString model.windowHeight) ]
            (List.map
                (toSvgLine { x = (toFloat model.windowWidth) / 2, y = (toFloat model.windowHeight) / 2 })
                (Turtle.lines (circleAndSquare 1 108 10 0.97 []))
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\{ height, width } -> WindowSize height width)


transform : Point -> Point -> Point
transform to from =
    let
        rotated =
            rotate 180 from
    in
        { x = rotated.x + to.x, y = rotated.y + to.y }


rotate : Int -> Point -> Point
rotate angle { x, y } =
    let
        rad =
            degrees (toFloat angle)

        newX =
            x * (cos rad) - y * (sin rad)

        newY =
            y * (cos rad) + x * (sin rad)
    in
        { x = newX, y = newY }


toSvgLine : Point -> Turtle.Line -> Svg.Svg msg
toSvgLine origo l =
    let
        start =
            transform origo l.start

        end =
            transform origo l.end
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


square : List Action
square =
    List.concat (List.repeat 4 [ Forward 200, Right 90 ])


circleAndSquare : Int -> Int -> Int -> Float -> List Action -> List Action
circleAndSquare current total angle s actions =
    if current == total then
        actions
    else
        let
            newSquare =
                List.append square [ Left angle, Scale (s ^ (toFloat current)) ]
        in
            circleAndSquare (current + 1) total angle s (List.append actions newSquare)
