module Main exposing (main)

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (succeed)
import LSystem exposing (..)
import Platform exposing (..)
import Result exposing (..)
import String exposing (..)
import Svg exposing (svg, line)
import Svg.Attributes exposing (viewBox, fill, stroke, x1, x2, y1, y2)
import Turtle exposing (..)


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { initialTokenString : String
    , rules : List Rule
    , system : List Token
    , iterations : Int
    , ruleInput : Maybe Rule
    , angle : Int
    }


type Msg
    = SetInitialToken String
    | SetRule String
    | AddRule
    | RemoveRule String
    | SetIterations String
    | SetAngle String


init : ( Model, Cmd Msg )
init =
    ( { initialTokenString = "FX"
      , rules = []
      , system = (tokenList "FX")
      , iterations = 1
      , ruleInput = Nothing
      , angle = 90
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            [ Svg.svg [ viewBox "0 0 1000 1000", Svg.Attributes.width "1000px" ]
                (List.map
                    toSvgLine
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
    { x = from.x + to.x, y = from.y + to.y }


toSvgLine : Line -> Svg.Svg msg
toSvgLine l =
    let
        to =
            { x = 250, y = 250 }

        start =
            transform to l.start

        end =
            transform to l.end
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
    Sub.none
