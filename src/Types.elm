module Types exposing (..)

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
    | UNKNOWN


type alias Symbol =
    { directions : Array Direction
    , corners : Array Point
    , startQuadrant : Quadrant
    , endQuadrant : Quadrant
    }


