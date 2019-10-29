module DataManipulation exposing (Direction, Point, curvature, fromMaybe, smoothing, thinning)

-- Types

import Array exposing (..)


type alias Point =
    ( Int, Int )


type Direction
    = UP
    | DOWN
    | LEFT
    | RIGHT
    | UNKNOWN



-- Manipulations


smoothing : Array Point -> Array Point -> Float -> Point -> Array Point
smoothing rawPoints smoothedPoints sf newPoint =
    let
        numberOfSmoothedPoints =
            Array.length smoothedPoints
    in
    if numberOfSmoothedPoints >= 1 then
        let
            -- 1. Take the last raw point
            -- 2. Take the last smoothed point
            lastRawPoint =
                fromMaybe <| Array.get (Array.length rawPoints - 1) rawPoints

            previousSmoothedPoint =
                if numberOfSmoothedPoints == 1 then
                    fromMaybe <| Array.get 0 smoothedPoints

                else
                    fromMaybe <| Array.get (numberOfSmoothedPoints - 1) smoothedPoints

            -- 3. Destruct the lastRawPoint Point:
            --     xRi, yRi - coordinates of the i-th raw point
            ( xRi, yRi ) =
                Tuple.mapBoth toFloat toFloat lastRawPoint

            -- 4. Destruct the previousSmoothedPoint:
            --     ySi, ySi - coordinates of the (i-1) smoothed point
            ( xSi_sub_1, ySi_sub_1 ) =
                Tuple.mapBoth toFloat toFloat previousSmoothedPoint

            -- 5. Calculate next smoothed point
            xSi =
                (sf * xSi_sub_1) + ((1 - sf) * xRi)

            ySi =
                (sf * ySi_sub_1) + ((1 - sf) * yRi)
        in
        -- 6. Add the last smoothed point to the smoothedList
        Array.push (Tuple.mapBoth round round ( xSi, ySi )) smoothedPoints

    else
        -- If  smoothedList is empty
        Array.push newPoint smoothedPoints


thinning : Array Point -> Array Point -> Int -> Array Point
thinning smoothedPoints thinnedPoints tf =
    let
        numberOfSmoothedPoints =
            Array.length smoothedPoints

        numberOfThinnedPoints =
            Array.length thinnedPoints
    in
    if Array.length thinnedPoints >= 1 then
        let
            -- 1. Take last smoothed Point
            ( xSi, ySi ) =
                fromMaybe <|
                    Array.get (numberOfSmoothedPoints - 1)
                        smoothedPoints

            -- 2. Take last thinned Point
            ( xTj_sub_1, yTj_sub_1 ) =
                fromMaybe <|
                    Array.get (numberOfThinnedPoints - 1)
                        thinnedPoints
        in
        -- 3. Calculate if the difference between last smoothed Point
        -- and last thinned Point is larger than the thinningFactor (tf)
        if
            (abs (xSi - xTj_sub_1) >= tf)
                || (abs (ySi - yTj_sub_1) >= tf)
        then
            -- 4. if the difference is larger add the last smoothed Point to
            -- thinnedPath
            Array.push ( xSi, ySi ) thinnedPoints

        else
            -- 5. if the difference is NOT larger return the old thinnedPath List
            thinnedPoints

    else
        -- if thinnedPath is empty
        Array.push
            (fromMaybe <|
                Array.get (numberOfSmoothedPoints - 1)
                    smoothedPoints
            )
            thinnedPoints


curvature : Array Point -> Array Direction -> Array Direction
curvature thinnedPath curvePath =
    let
        numberOfThinnedPoints =
            Array.length thinnedPath
    in
    if numberOfThinnedPoints >= 2 then
        let
            xy_Tj =
                fromMaybe <| Array.get (numberOfThinnedPoints - 1) thinnedPath

            xy_Tj_sub_1 =
                fromMaybe <| Array.get (numberOfThinnedPoints - 2) thinnedPath

            numberOfDirections =
                Array.length curvePath

            deg =
                convertFromCartesianToPolar xy_Tj xy_Tj_sub_1

            direction =
                convertFromPolarToDirection deg
        in
        if Array.isEmpty curvePath then
            Array.push direction curvePath

        else
            let
                lastDirection =
                    fromMaybeDirection <| Array.get (numberOfDirections - 1) curvePath
            in
            if lastDirection == direction then
                curvePath

            else
                Array.push direction curvePath

    else
        curvePath


convertFromCartesianToPolar : Point -> Point -> Float
convertFromCartesianToPolar ( xTj, yTj ) ( xTj_sub_1, yTj_sub_1 ) =
    let
        x =
            toFloat <| (xTj_sub_1 - xTj)

        y =
            toFloat <| (yTj_sub_1 - yTj)

        theta_rad =
            atan2 y x

        theta_degree =
            if theta_rad < 0 then
                theta_rad + degrees 360

            else
                theta_rad
    in
    theta_degree


convertFromPolarToDirection : Float -> Direction
convertFromPolarToDirection deg =
    if (degrees 45 <= deg) && (deg < degrees 135) then
        UP

    else if (degrees 135 <= deg) && (deg < degrees 225) then
        RIGHT

    else if (degrees 225 <= deg) && (deg < degrees 315) then
        DOWN

    else
        LEFT


fromMaybe : Maybe Point -> Point
fromMaybe x =
    case x of
        Just y ->
            y

        Nothing ->
            ( 0, 0 )


fromMaybeDirection : Maybe Direction -> Direction
fromMaybeDirection direction =
    case direction of
        Just d ->
            d

        Nothing ->
            UNKNOWN
