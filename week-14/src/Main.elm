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
import Time exposing (..)


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
    , currentIteration : Int
    , iterations : Int
    , ruleInput : Maybe Rule
    , angle : Int
    }


type Msg
    = Tick Time
    | SetInitialToken String
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
      , currentIteration = 0
      , iterations = 17
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
                    , system = (tokenList filteredString)
                    , currentIteration = 0
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
                        , system = (tokenList model.initialTokenString)
                        , currentIteration = 0
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        RemoveRule rule ->
            ( { model
                | rules = List.filter (\item -> (ruleString item) /= rule) model.rules
                , system = (tokenList model.initialTokenString)
                , currentIteration = 0
              }
            , Cmd.none
            )

        SetIterations i ->
            ( { model
                | iterations = (withDefault 0 (toInt i))
                , system = (tokenList model.initialTokenString)
                , currentIteration = 0
              }
            , Cmd.none
            )

        SetAngle a ->
            ( { model
                | angle = (withDefault 90 (toInt a))
                , system = (tokenList model.initialTokenString)
                , currentIteration = 0
              }
            , Cmd.none
            )

        Tick _ ->
            if model.currentIteration < model.iterations then
                ( { model
                    | system = replace model.system model.rules
                    , currentIteration = model.currentIteration + 1
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )


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
            [ text (toString model.currentIteration)
            ]
        , div []
            [ text (tokenString model.system)
            ]
        ]


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
    Time.every (100 * millisecond) Tick
