module Turtle exposing (..)

import Array exposing (..)
import List exposing (..)


type Action
    = Forward Int
    | Left Int
    | Right Int
    | PenDown
    | PenUp


type alias Point =
    { x : Int, y : Int }


type alias Line =
    ( Point, Point )


type alias State =
    { lines : Array Line
    , penDown : Bool
    , currentLocation : Point
    , heading : Int
    }


eval : Action -> State -> State
eval action state =
    case action of
        Forward distance ->
            let
                current =
                    state.currentLocation

                dx =
                    round ((toFloat distance) * sin (degrees (toFloat state.heading)))

                dy =
                    round ((toFloat distance) * cos (degrees (toFloat state.heading)))

                new =
                    { x = current.x + dx, y = current.y + dy }
            in
                if state.penDown then
                    { state | currentLocation = new, lines = push ( current, new ) state.lines }
                else
                    { state | currentLocation = new }

        Left angle ->
            { state | heading = (state.heading - angle + 360) % 360 }

        Right angle ->
            { state | heading = (state.heading + angle + 360) % 360 }

        PenUp ->
            { state | penDown = False }

        PenDown ->
            { state | penDown = True }


fold : List Action -> State
fold actions =
    foldState actions initialState


foldState : List Action -> State -> State
foldState actions state =
    List.foldl eval state actions


initialState : State
initialState =
    { lines = Array.empty
    , penDown = True
    , currentLocation = { x = 0, y = 0 }
    , heading = 0
    }


lines : List Action -> List Line
lines actions =
    (toList (fold actions).lines)
