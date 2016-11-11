module Main exposing (main)

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (succeed)
import LSystem exposing (..)
import Platform exposing (..)
import Result exposing (..)
import String exposing (toUpper, toInt)
import Svg exposing (svg, line)
import Svg.Attributes exposing (viewBox, fill, stroke, x1, x2, y1, y2)
import Turtle exposing (..)
import Window exposing (..)
import Task exposing (perform)


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { canvasHeight : Int
    , canvasWidth : Int
    , initialTokenString : String
    , rules : List Rule
    , system : List Token
    , iterations : Int
    , ruleInput : Maybe Rule
    , angle : Int
    }


type Msg
    = WindowSize Int Int
    | SetInitialToken String
    | SetRule String
    | AddRule
    | RemoveRule String
    | SetIterations String
    | SetAngle String


init : ( Model, Cmd Msg )
init =
    ( { canvasHeight = 0
      , canvasWidth = 0
      , initialTokenString = "FX"
      , rules = [ { token = 'X', replacement = (tokenList "X+YF+") }, { token = 'Y', replacement = (tokenList "-FX-Y") } ]
      , system = (tokenList "FX")
      , iterations = 0
      , ruleInput = Nothing
      , angle = 90
      }
    , Task.perform (\_ -> WindowSize 0 0) (\{ height, width } -> WindowSize height width) Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowSize h w ->
            ( { model
                | canvasHeight = h - 200
                , canvasWidth = w - 100
              }
            , Cmd.none
            )

        SetInitialToken str ->
            let
                filteredString =
                    (String.toUpper str |> String.filter isAllowed)
            in
                ( { model
                    | initialTokenString = filteredString
                    , system = lsystem (tokenList filteredString) model.rules model.iterations
                  }
                , Cmd.none
                )

        SetRule rule ->
            ( { model
                | ruleInput = fromString rule
              }
            , Cmd.none
            )

        AddRule ->
            case model.ruleInput of
                Just rule ->
                    ( { model
                        | rules = rule :: model.rules
                        , ruleInput = Nothing
                        , system = lsystem (tokenList model.initialTokenString) model.rules model.iterations
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        RemoveRule rule ->
            ( { model
                | rules = List.filter (\item -> (ruleString item) /= rule) model.rules
                , system = lsystem (tokenList model.initialTokenString) model.rules model.iterations
              }
            , Cmd.none
            )

        SetIterations i ->
            let
                newIterations =
                    (withDefault 0 (toInt i))
            in
                ( { model
                    | iterations = newIterations
                    , system = lsystem (tokenList model.initialTokenString) model.rules newIterations
                  }
                , Cmd.none
                )

        SetAngle a ->
            ( { model
                | angle = (withDefault 90 (toInt a))
                , system = lsystem (tokenList model.initialTokenString) model.rules model.iterations
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "This is a highly inefficient rendering of an L-System. 17 iterations for the dragon curve takes ~5-10 seconds."
            , br [] []
            , text (" Allowed tokens: " ++ (tokenString allowedTokens))
            ]
        , div []
            [ label [ for "initial" ] [ text "Initial tokens" ]
            , input [ id "initial", value model.initialTokenString, onInput SetInitialToken ] []
            ]
        , div []
            [ label [ for "iteration" ] [ text "Iterations" ]
            , input [ id "iterations", value (toString model.iterations), onInput SetIterations ] []
            ]
        , div []
            [ label [ for "angle" ] [ text "Angle" ]
            , input [ id "angle", value (toString model.angle), onInput SetAngle ] []
            ]
        , div []
            [ label [ for "rule" ] [ text "Rule" ]
            , input [ id "rule", value (Maybe.map ruleString model.ruleInput |> Maybe.withDefault ""), onInput SetRule ] []
            , button [ onClick AddRule ] [ text "Add rule" ]
            ]
        , div []
            [ ul [] (List.map ruleItem model.rules)
            ]
        , div []
            [ Svg.svg [ Svg.Attributes.width (toString model.canvasWidth), Svg.Attributes.height (toString model.canvasHeight) ]
                (List.map
                    (toSvgLine { x = (toFloat model.canvasWidth) / 2, y = (toFloat model.canvasHeight) / 2 })
                    (toLines model.system)
                )
            ]
        ]


toAction : Token -> Maybe Action
toAction token =
    case token of
        'F' ->
            Just (Forward 1)

        '+' ->
            Just (Right 90)

        '-' ->
            Just (Left 90)

        _ ->
            Nothing


toLines : List Token -> List Line
toLines tokens =
    Turtle.lines (List.filterMap toAction tokens)


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


toSvgLine : Point -> Line -> Svg.Svg msg
toSvgLine center l =
    let
        start =
            transform center l.start

        end =
            transform center l.end
    in
        Svg.line [ fill "none", stroke "black", x1 (toString start.x), x2 (toString end.x), y1 (toString start.y), y2 (toString end.y) ] []


ruleItem : Rule -> Html Msg
ruleItem rule =
    li [] [ text (ruleString rule), button [ on "click" (succeed (RemoveRule (ruleString rule))) ] [ text "Remove" ] ]


tokenString : List Token -> String
tokenString tokens =
    String.fromList tokens


isAllowed : Char -> Bool
isAllowed token =
    List.member token allowedTokens


allowedTokens : List Token
allowedTokens =
    [ 'F', 'X', 'Y', '+', '-' ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\{ height, width } -> WindowSize height width)
