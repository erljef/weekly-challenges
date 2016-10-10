module Circle exposing (Point, circle)

import List.Extra exposing (unfoldr)


type alias Point =
    ( Int, Int )


{-| Using the midpoint circle algorithm to generate a list of coordinates.
-}
circle : Int -> List Point
circle radius =
    let
        points =
            List.concatMap generatePoints (unfoldr step initialValues)

        generatePoints point =
            let
                x =
                    fst point

                y =
                    snd point
            in
                [ ( x, y )
                , ( -x, y )
                , ( -x, -y )
                , ( x, -y )
                , ( y, x )
                , ( -y, x )
                , ( y, -x )
                , ( -y, -x )
                ]

        initialPoints =
            [ ( 0, radius ), ( 0, -radius ), ( radius, 0 ), ( -radius, 0 ) ]

        initialValues =
            ( 1 - radius, 1, (-2) * radius, 0, radius )

        step ( f, ddf_x, ddf_y, x, y ) =
            if x >= y then
                Nothing
            else
                let
                    ddf_x' =
                        ddf_x + 2

                    x' =
                        x + 1

                    ( f', ddf_y', y' ) =
                        if (f >= 0) then
                            ( f + ddf_y + 2 + ddf_x + 1, ddf_y + 2, y - 1 )
                        else
                            ( f + ddf_x, ddf_y, y )
                in
                    Just ( ( x', y' ), ( f', ddf_x', ddf_y', x', y' ) )
    in
        List.sortWith comparator (List.concat [ initialPoints, points ])


{-| Comparator that compares the angle of two points, effectively sorting an array of points in drawing order.
-}
comparator : Point -> Point -> Order
comparator a b =
    let
        angle_a =
            (snd (toPolar ( toFloat (fst a), toFloat (snd a) )))

        angle_b =
            (snd (toPolar ( toFloat (fst b), toFloat (snd b) )))
    in
        compare angle_a angle_b
