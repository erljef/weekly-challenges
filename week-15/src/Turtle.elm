module Turtle exposing (Action(..), Point, Line, lines)

import Array exposing (..)
import List exposing (..)


{-| The actions the turtle can perform
-}
type Action
    = Forward Int
    | Left Int
    | Right Int
    | PenDown
    | PenUp
    | Color String
    | Width Int
    | Push
    | Pop
    | Scale Float


type alias Point =
    { x : Int, y : Int }


type alias Line =
    { start : Point
    , end : Point
    , color : String
    , width : Int
    }


type alias State =
    { lines : Array Line
    , currentState : List TurtleState
    }


type alias TurtleState =
    { penDown : Bool
    , currentLocation : Point
    , heading : Int
    , currentColor : String
    , currentWidth : Int
    , currentScale : Float
    }


{-| Get the current TurtleState from the "global" State
-}
currentState : State -> TurtleState
currentState state =
    Maybe.withDefault initialState (head state.currentState)


{-| Create a new State where the current TurtleState is replaced
-}
newState state new =
    { state | currentState = new :: (drop 1 state.currentState) }


{-| Evaluate a single action
-}
eval : Action -> State -> State
eval action state =
    let
        current =
            currentState state
    in
        case action of
            Forward d ->
                let
                    location =
                        current.currentLocation

                    distance =
                        round (current.currentScale * (toFloat d))

                    dx =
                        round ((toFloat distance) * sin (degrees (toFloat current.heading)))

                    dy =
                        round ((toFloat distance) * cos (degrees (toFloat current.heading)))

                    new =
                        { x = location.x + dx, y = location.y + dy }

                    w =
                        round (current.currentScale * (toFloat current.currentWidth))
                in
                    if current.penDown then
                        newState
                            { state
                                | lines = push { start = location, end = new, color = current.currentColor, width = w } state.lines
                            }
                            { current | currentLocation = new }
                    else
                        newState state { current | currentLocation = new }

            Left angle ->
                newState state { current | heading = (current.heading + angle + 360) % 360 }

            Right angle ->
                newState state { current | heading = (current.heading - angle + 360) % 360 }

            PenUp ->
                newState state { current | penDown = False }

            PenDown ->
                newState state { current | penDown = True }

            Color c ->
                newState state { current | currentColor = c }

            Width w ->
                newState state { current | currentWidth = w }

            Push ->
                { state | currentState = current :: state.currentState }

            Pop ->
                { state | currentState = (drop 1 state.currentState) }

            Scale s ->
                newState state { current | currentScale = s }


{-| Evaluate a list of actions
-}
fold : List Action -> State
fold actions =
    foldState actions { lines = Array.empty, currentState = [] }


foldState : List Action -> State -> State
foldState actions state =
    List.foldl eval state actions


initialState : TurtleState
initialState =
    { penDown = True
    , currentLocation = { x = 0, y = 0 }
    , heading = 0
    , currentColor = "black"
    , currentWidth = 1
    , currentScale = 1.0
    }


{-| Get the lines created by a list of actions
-}
lines : List Action -> List Line
lines actions =
    (toList (fold actions).lines)
