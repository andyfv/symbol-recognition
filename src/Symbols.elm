module Symbols exposing (..)

import Array exposing (toList)
import Types exposing (..)

recognizeSymbolDown : Symbol -> String
recognizeSymbolDown symbol =
    let
        pattern = symbol.directions
            |> toList
    in
        case pattern of
            [DOWN] ->
                "I"

            [DOWN,RIGHT, UP, LEFT] ->
                "D"


            [DOWN,RIGHT] ->
                "L"

            [DOWN,RIGHT, UP, DOWN, LEFT] ->
                "Y"

            [DOWN,RIGHT, UP, DOWN, RIGHT, LEFT] ->
                "Y"

            [DOWN,UP, RIGHT, UP, DOWN] ->
                "H"

            [DOWN,UP, DOWN] ->
                "K"

            [DOWN,UP, DOWN, LEFT, DOWN, RIGHT] ->
                "K"

            [DOWN,UP, LEFT, DOWN, RIGHT] ->
                "K"

            [DOWN,UP, DOWN, RIGHT] ->
                "K"

            [DOWN,UP, DOWN, UP] ->
                "W"

            [DOWN,RIGHT, UP ,DOWN] ->
                "4"

            _ ->
                "Unknown"



recognizeSymbolRight : Symbol -> String
recognizeSymbolRight symbol =
    let
        pattern = symbol.directions
            |> toList
    in
        case pattern of
            [RIGHT, DOWN, LEFT] ->
                "J"

            [RIGHT, DOWN, LEFT, UP] ->
                "J"

            [RIGHT, DOWN, RIGHT] ->
                "Z"

            [RIGHT, LEFT, RIGHT] ->
                "Z"

            [RIGHT, DOWN,LEFT, DOWN, RIGHT] ->
                "Z"

            [RIGHT, DOWN,LEFT, RIGHT] ->    -- TODO: Check if it is "2"
                "Z"

            [RIGHT,DOWN, LEFT, RIGHT, DOWN, LEFT] ->
                "3"

            [RIGHT, DOWN] ->
                "7"

            _ ->
                "Unknown"


recognizeSymbolLeft : Symbol -> String
recognizeSymbolLeft symbol =
    let
        pattern = symbol.directions
            |> toList
    in
        case pattern of
            [LEFT, DOWN] ->
                "F"

            [LEFT, DOWN, RIGHT, LEFT, DOWN, RIGHT] ->
                "E"

            [LEFT, DOWN, RIGHT, UP, LEFT] ->    -- TODO: Check if is "O"
                "G"

            [LEFT, DOWN, RIGHT, DOWN, LEFT] ->  -- TODO: Check if it is "5"
                "S"

            [LEFT, DOWN, RIGHT, UP, LEFT, DOWN] ->
                "6"

            [LEFT, DOWN, RIGHT, DOWN, LEFT, UP, RIGHT, UP, LEFT] ->
                "8"

            [LEFT,DOWN,RIGHT,DOWN,LEFT,UP,LEFT] ->
                "8"

            [LEFT,DOWN,RIGHT,DOWN,LEFT,UP,RIGHT,UP] ->
                "8"

            [LEFT,DOWN,RIGHT,DOWN,LEFT,UP] ->
                "8"

            [LEFT, DOWN, RIGHT, UP, DOWN, LEFT] ->
                "9"

            _ ->
                "Unknown"


recognizeSymbolUp : Symbol -> String
recognizeSymbolUp symbol =
    let
        pattern = symbol.directions
            |> toList
    in
        case pattern of
            [UP, DOWN] ->
                "1"

            []

            _ ->
                "Unknown"