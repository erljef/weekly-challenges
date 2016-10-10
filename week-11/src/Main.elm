module Main exposing (main)

import Circle exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, stroke, strokeWidth, viewBox)


main : Program Never
main =
    Html.beginnerProgram
        { model = { line = Solid, x = 100, y = 100, radius = 50, start = 0, end = 360 }
        , view = view
        , update = update
        }


type LineType
    = Solid
    | Dotted
    | Dashed


type alias Model =
    { line : LineType
    , x : Int
    , y : Int
    , radius : Int
    , start : Int
    , end : Int
    }


type Msg
    = SetLineType LineType
    | SetX String
    | SetY String
    | SetSize String
    | SetStart String
    | SetEnd String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetLineType t ->
            { model | line = t }

        SetX p ->
            { model | x = Result.withDefault 0 (toInt p) }

        SetY p ->
            { model | y = Result.withDefault 0 (toInt p) }

        SetSize s ->
            { model | radius = Result.withDefault 0 (toInt s) }

        SetStart s ->
            { model | start = Result.withDefault 0 (toInt s) }

        SetEnd e ->
            { model | end = Result.withDefault 360 (toInt e) }


drawPoint : Point -> Svg a
drawPoint point =
    rect
        [ x (toString (fst point))
        , y (toString (snd point))
        , Svg.Attributes.width "1"
        , Svg.Attributes.height "1"
        , stroke "#000000"
        , strokeWidth "1"
        ]
        []


lineType : Model -> List Point -> List Point
lineType model =
    case model.line of
        Solid ->
            identity

        Dotted ->
            dotted

        Dashed ->
            dashed


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [] [ text "X", input [ type' "text", onInput SetX, value (toString model.x) ] [] ]
            , label [] [ text "Y", input [ type' "text", onInput SetY, value (toString model.y) ] [] ]
            , label [] [ text "Radius", input [ type' "text", onInput SetSize, value (toString model.radius) ] [] ]
            , label [] [ text "Start", input [ type' "text", onInput SetStart, value (toString model.start) ] [] ]
            , label [] [ text "End", input [ type' "text", onInput SetEnd, value (toString model.end) ] [] ]
            , fieldset []
                [ label [] [ input [ type' "radio", onClick (SetLineType Solid), checked (model.line == Solid) ] [], text "Solid" ]
                , label [] [ input [ type' "radio", onClick (SetLineType Dotted), checked (model.line == Dotted) ] [], text "Dotted" ]
                , label [] [ input [ type' "radio", onClick (SetLineType Dashed), checked (model.line == Dashed) ] [], text "Dashed" ]
                ]
            ]
        , Svg.svg [ viewBox "0 0 500 500", Svg.Attributes.width "500px" ]
            (List.map drawPoint ((lineType model) (List.map (moveTo ( model.x, model.y )) (arc model (circle model.radius)))))
        ]


{-| Transform a point to another origin.
-}
moveTo : Point -> Point -> Point
moveTo origin point =
    ( (fst origin) + (fst point), (snd origin) + (snd point) )


{-| Filter a list of points to represent a dashed line.
-}
dashed : List Point -> List Point
dashed points =
    List.filterMap dash (List.indexedMap (,) points)


{-| Filter a list of points to represent a dotted line.
-}
dotted : List Point -> List Point
dotted points =
    List.filterMap (nth 5) (List.indexedMap (,) points)


nth : Int -> ( Int, Point ) -> Maybe Point
nth number elem =
    if (fst elem) % number == 0 then
        Just (snd elem)
    else
        Nothing


dash : ( Int, Point ) -> Maybe Point
dash elem =
    if List.member (rem (fst elem) 10) [ 0, 1, 2, 3 ] then
        Nothing
    else
        Just (snd elem)


{-| Filter a list of points to represent only part of a circle.
-}
arc : Model -> List Point -> List Point
arc model points =
    List.filter (isInArc model) points


isInArc : Model -> Point -> Bool
isInArc model point =
    let
        a_degrees =
            (snd (toPolar ( toFloat (fst point), toFloat (snd point) )))
    in
        a_degrees >= (toFloat (model.start - 180)) && a_degrees < (toFloat (model.end - 180))
