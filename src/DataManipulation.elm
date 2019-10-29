module DataManipulation exposing (Point, fromMaybe, smoothing, thinning)


type alias Point =
    ( Int, Int )


smoothing : List Point -> List Point -> Float -> Point -> List Point
smoothing rawList smoothedList sf newPoint =
    if List.length smoothedList > 1 then
        let
            -- 1. Take the last raw point
            -- 2. Take the last smoothed point
            lastRawPoint =
                fromMaybe <| List.head rawList

            previousSmoothedPoint =
                fromMaybe <| List.head smoothedList

            -- 3. Decode the lastRawPoint Point tuple to:
            --     xRi, yRi - coordinates of the i-th raw point
            ( xRi, yRi ) =
                Tuple.mapBoth toFloat toFloat lastRawPoint

            -- 4. Destruct the previousSmoothedPoint tuple to:
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
        Tuple.mapBoth round round ( xSi, ySi ) :: smoothedList

    else
        -- If  smoothedList is empty
        newPoint :: smoothedList


thinning : List Point -> List Point -> Int -> List Point
thinning smoothedPath thinnedPath tf =
    if List.length thinnedPath > 1 then
        let
            -- 1. Take last smoothed Point
            ( xSi, ySi ) =
                fromMaybe <| List.head smoothedPath

            -- 2. Take last thinned Point
            ( xTj_sub_1, yTj_sub_1 ) =
                fromMaybe <| List.head thinnedPath
        in
        -- 3. Calculate if the difference between last smoothed Point
        -- and last thinned Point is larger than the thinningFactor (tf)
        if
            (abs (xSi - xTj_sub_1) >= tf)
                || (abs (ySi - yTj_sub_1) >= tf)
        then
            -- 4. if the difference is larger add the last smoothed Point to
            -- thinnedPath
            ( xSi, ySi ) :: thinnedPath

        else
            -- 5. if the difference is NOT larger return the old thinnedPath List
            thinnedPath

    else
        -- if thinnedPath is empty
        (fromMaybe <| List.head smoothedPath) :: thinnedPath


fromMaybe : Maybe Point -> Point
fromMaybe x =
    case x of
        Just y ->
            y

        Nothing ->
            ( 0, 0 )
