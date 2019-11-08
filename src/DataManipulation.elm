module DataManipulation exposing (detectCorners, getDirection, getStartAndEndPosition, recognizeSymbol, smoothing, thinning)

-- Types

import Array exposing (..)
import Symbols exposing (..)
import Types exposing (..)



-- Manipulations
-- Smoothing


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
                fromMaybePoint <| Array.get (Array.length rawPoints - 1) rawPoints

            previousSmoothedPoint =
                if numberOfSmoothedPoints == 1 then
                    fromMaybePoint <| Array.get 0 smoothedPoints

                else
                    fromMaybePoint <| Array.get (numberOfSmoothedPoints - 1) smoothedPoints

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



-- Thinning


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
                fromMaybePoint <|
                    Array.get (numberOfSmoothedPoints - 1)
                        smoothedPoints

            -- 2. Take last thinned Point
            ( xTj_sub_1, yTj_sub_1 ) =
                fromMaybePoint <|
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
            (fromMaybePoint <|
                Array.get (numberOfSmoothedPoints - 1)
                    smoothedPoints
            )
            thinnedPoints



-- Directions
-- Note: This also returns UNKNOWN if the direction is in the hysteresis zones


getDirection : Array Point -> Array Direction -> Array Direction
getDirection thinnedPath directionPath =
    let
        numberOfThinnedPoints =
            Array.length thinnedPath

        numberOfDirections =
            Array.length directionPath

        xy_Tj =
            fromMaybePoint <| Array.get (numberOfThinnedPoints - 1) thinnedPath

        xy_Tj_sub_1 =
            fromMaybePoint <| Array.get (numberOfThinnedPoints - 2) thinnedPath

        deg =
            convertFromCartesianToPolar xy_Tj xy_Tj_sub_1

        direction =
            convertFromPolarToDirection deg
    in
    if
        numberOfThinnedPoints
            >= 2
            && numberOfDirections
            < 8
            && direction
            /= UNKNOWN
    then
        if Array.isEmpty directionPath then
            Array.push direction directionPath

        else
            let
                lastDirection =
                    fromMaybeDirection <| Array.get (numberOfDirections - 1) directionPath
            in
            if lastDirection == direction then
                directionPath

            else
                Array.push direction directionPath

    else
        directionPath


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
            -- Since atan2 returns negative values for III and IV quadrant
            -- just add 2*pi if the return value is negative
            -- to convert it to '0 - 360' polar system
            if theta_rad < 0 then
                theta_rad + (2 * pi)

            else
                theta_rad
    in
    theta_degree



{-
   Normally detection zones will be between
       - UP : (45 <= deg < 135)
       - DOWN : (225 <= deg < 315)
       - LEFT : (315 <= deg < 45) -- Note: Reversed with RIGHT
       - RIGHT : (135 <= deg < 225) -- Note: Reversed with LEFT
   With the added hysteresis zones of 16deg around (45, 135, 225, 315):
       - UP : (53 <= deg < 137)
       - DOWN : (233 <= deg < 307)
       - LEFT : (323 <= deg < 53) -- Note: Reversed with RIGHT
       - RIGHT : (143 <= deg < 218) -- Note: Reversed with LEFT
-}


convertFromPolarToDirection : Float -> Direction
convertFromPolarToDirection deg =
    if (degrees 53 <= deg) && (deg < degrees 137) then
        UP

    else if (degrees 143 <= deg) && (deg < degrees 217) then
        RIGHT

    else if (degrees 233 <= deg) && (deg < degrees 307) then
        DOWN

    else if (degrees 323 <= deg) && (deg < 37) then
        LEFT

    else
        UNKNOWN



-- Corners


detectCorners : Array Point -> Array Point -> Float -> Array Point
detectCorners thinnedPoints cornersPoints cornerThreshold =
    let
        numberOfThinnedPoints =
            Array.length thinnedPoints
    in
    if numberOfThinnedPoints > 5 then
        let
            -- Get last 5 thinned points
            xyTj =
                getPointReversed thinnedPoints numberOfThinnedPoints 0

            xyTj_sub_1 =
                getPointReversed thinnedPoints numberOfThinnedPoints 1

            xyTj_sub_2 =
                getPointReversed thinnedPoints numberOfThinnedPoints 2

            xyTj_sub_3 =
                getPointReversed thinnedPoints numberOfThinnedPoints 3

            xyTj_sub_4 =
                getPointReversed thinnedPoints numberOfThinnedPoints 4

            -- Get the vectors based on the last 5 points
            vec1 =
                getVector xyTj xyTj_sub_1

            vec2 =
                getVector xyTj_sub_1 xyTj_sub_2

            vec3 =
                getVector xyTj_sub_3 xyTj_sub_2

            vec4 =
                getVector xyTj_sub_4 xyTj_sub_3

            angleBetweenVec1Vec2 =
                getAngleBetweenTwoVectors vec1 vec2

            angleBetweenVec3Vec4 =
                getAngleBetweenTwoVectors vec3 vec4

            angleBetweenVec1Vec3 =
                getAngleBetweenTwoVectors vec1 vec3
        in
        if
            angleBetweenVec1Vec2
                <= degrees 45
                && angleBetweenVec3Vec4
                <= degrees 45
                && angleBetweenVec1Vec3
                <= degrees cornerThreshold
                && checkForDuplicate cornersPoints xyTj_sub_2
                == False
        then
            Array.push xyTj_sub_2 cornersPoints

        else
            cornersPoints

    else
        cornersPoints


checkForDuplicate : Array Point -> Point -> Bool
checkForDuplicate cornerPoints newCorner =
    let
        numberOfCorners =
            Array.length cornerPoints
    in
    if numberOfCorners == 0 then
        False

    else
        let
            lastCorner =
                getPointReversed cornerPoints numberOfCorners 0
        in
        if lastCorner /= newCorner then
            False

        else
            True


getVector : Point -> Point -> ( Float, Float )
getVector ( xTj, yTj ) ( xTj_sub_1, yTj_sub_1 ) =
    let
        --        x = toFloat <| if xTj > xTj_sub_1
        --            then   (xTj - xTj_sub_1)
        --            else   -(xTj - xTj_sub_1)
        x =
            toFloat (xTj - xTj_sub_1)

        --        y = toFloat <| if yTj > yTj_sub_1
        --            then   -(yTj - yTj_sub_1)
        --            else   (yTj - yTj_sub_1)
        y =
            toFloat (yTj_sub_1 - yTj)
    in
    ( x, y )


getAngleBetweenTwoVectors : ( Float, Float ) -> ( Float, Float ) -> Float
getAngleBetweenTwoVectors ( xA, yA ) ( xB, yB ) =
    let
        dotProduct =
            (xA * xB) + (yA * yB)

        vectorMagnitudeA =
            sqrt ((xA ^ 2) + (yA ^ 2))

        vectorMagnitudeB =
            sqrt ((xB ^ 2) + (yB ^ 2))
    in
    acos (dotProduct / (vectorMagnitudeA * vectorMagnitudeB))


getPointReversed : Array Point -> Int -> Int -> Point
getPointReversed array arrayLength position =
    fromMaybePoint <|
        Array.get ((arrayLength - 1) - position) array



-- Get Start and End Positions (startPosition, EndPosition)


getStartAndEndPosition : Array Point -> ( Quadrant, Quadrant )
getStartAndEndPosition thinnedInput =
    let
        startingPositon =
            fromMaybePoint <| Array.get 0 thinnedInput

        endingPositon =
            fromMaybePoint <| Array.get (Array.length thinnedInput - 1) thinnedInput

        listThinnedInput =
            toList thinnedInput

        listX =
            List.map (\( x, _ ) -> x) listThinnedInput

        listY =
            List.map (\( _, y ) -> y) listThinnedInput

        left =
            fromMaybeNumber <| List.minimum listX

        right =
            fromMaybeNumber <| List.maximum listX

        top =
            fromMaybeNumber <| List.minimum listY

        bottom =
            fromMaybeNumber <| List.maximum listY

        width =
            right - left

        height =
            bottom - top

        extremes : Extremes
        extremes =
            { top = toFloat top
            , bottom = toFloat bottom
            , left = toFloat left
            , right = toFloat right
            , width = toFloat width
            , height = toFloat height
            }

        startPointQuadrant =
            getPositionQuadrant startingPositon extremes

        endPointQuadrant =
            getPositionQuadrant endingPositon extremes
    in
    ( startPointQuadrant, endPointQuadrant )


getPositionQuadrant : Point -> Extremes -> Quadrant
getPositionQuadrant ( x, y ) extremes =
    let
        halfWidth =
            extremes.width / 2

        halfHeight =
            extremes.height / 2
    in
    if
        y
            <= floor (extremes.bottom - halfHeight)
            && x
            <= floor (extremes.right - halfWidth)
    then
        II

    else if
        x
            < floor (extremes.right - halfWidth)
            && y
            > floor (extremes.bottom - halfHeight)
    then
        III

    else if
        x
            >= floor (extremes.right - halfWidth)
            && y
            >= floor (extremes.bottom - halfHeight)
    then
        IV
        --    else x > (extremes.right - (extremes.width / 2))
        --    && y < (extremes.bottom - (extremes.height / 2))

    else
        I


recognizeSymbol : Symbol -> String
recognizeSymbol symbol =
    let
        firstDirection =
            fromMaybeDirection <| Array.get 0 symbol.directions
    in
    case firstDirection of
        DOWN ->
            recognizeSymbolDown symbol

        UP ->
            recognizeSymbolUp symbol

        LEFT ->
            recognizeSymbolLeft symbol

        RIGHT ->
            recognizeSymbolRight symbol

        UNKNOWN ->
            "Unknown"
