module Types exposing (Direction(..), Extremes, Point, Quadrant(..), Symbol, fromMaybeDirection, fromMaybeList, fromMaybeNumber, fromMaybePoint)

import Array exposing (Array)


type alias Point =
    ( Int, Int )


type alias Extremes =
    { top : Float
    , bottom : Float
    , left : Float
    , right : Float
    , width : Float
    , height : Float
    }



-- Quadrants are the same as in Cartesian coord system


type Quadrant
    = I
    | II
    | III
    | IV


type Direction
    = UP
    | DOWN
    | LEFT
    | RIGHT
    | DISCARDED
    | EMPTY


type alias Symbol =
    { mainDirections : List Direction
    , secondaryDirections : List Direction
    , corners : Array Point
    , startQuadrant : Quadrant
    , endQuadrant : Quadrant
    }


fromMaybeNumber : Maybe number -> number
fromMaybeNumber number =
    case number of
        Just y ->
            y

        Nothing ->
            0


fromMaybePoint : Maybe Point -> Point
fromMaybePoint x =
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
            DISCARDED


fromMaybeList : Maybe (List a) -> List a
fromMaybeList list =
    case list of
        Just l ->
            l

        Nothing ->
            []
