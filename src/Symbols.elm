module Symbols exposing (recognizeSymbolDown, recognizeSymbolLeft, recognizeSymbolRight, recognizeSymbolUp)

import Array exposing (toList)
import Types exposing (..)


--symbolQuadrants =
--    { a = (III, III)
--    , b =
--    }

recognizeSymbolUp : Symbol -> String
recognizeSymbolUp symbol =
    let
        mainDirections = symbol.mainDirections
    in
    case mainDirections of
        [ UP, DOWN, EMPTY, EMPTY ] ->
            "1"

        [ UP, DOWN, UP, EMPTY ] ->
            sym_A_N symbol

        [ UP, RIGHT, DOWN, UP ] ->
            sym_A_2_3 symbol

        [ UP, DOWN, UP, DOWN ] ->
            "M"

        [ UP, RIGHT, DOWN, RIGHT] ->
            sym_N_M_R_3 symbol

        [ UP, RIGHT, DOWN, LEFT, RIGHT, DOWN, LEFT ] ->
            -- TODO: Check "3" or "B"
            "B"

        [ UP, RIGHT, DOWN, LEFT ] ->
            "P"

        [ UP, RIGHT, DOWN, LEFT, RIGHT, DOWN ] ->
            "R"

        [ UP, LEFT, DOWN, RIGHT ] ->
            "C"

        [ UP, RIGHT, DOWN, LEFT, UP ] ->
            "2"

        _ ->
            "Unknown"



recognizeSymbolDown : Symbol -> String
recognizeSymbolDown symbol =
    let
        mainDirections = symbol.mainDirections
    in
    case mainDirections of
        [ DOWN ] ->
            "I"

        [ DOWN, RIGHT, UP, LEFT ] ->
            "D"

        [ DOWN, RIGHT ] ->
            "L"

        [ DOWN, RIGHT, UP, DOWN, LEFT ] ->
            "Y"

        [ DOWN, RIGHT, UP, DOWN, RIGHT, LEFT ] ->
            "Y"

        [ DOWN, UP, RIGHT, UP, DOWN ] ->
            "H"

        [ DOWN, UP, DOWN ] ->
            "K"

        [ DOWN, UP, DOWN, LEFT, DOWN, RIGHT ] ->
            "K"

        [ DOWN, UP, LEFT, DOWN, RIGHT ] ->
            "K"

        [ DOWN, UP, DOWN, RIGHT, DOWN] ->
            "K"

        [DOWN,UP,RIGHT,DOWN,RIGHT,DOWN ] ->
            "K"

        [ DOWN, UP, DOWN, RIGHT ] ->
            "K"

        [ DOWN, UP, DOWN, UP ] ->
            "W"

        [ DOWN, RIGHT, UP, DOWN ] ->
            "4"

        _ ->
            "Unknown"


recognizeSymbolRight : Symbol -> String
recognizeSymbolRight symbol =
    let
        mainDirections = symbol.mainDirections
    in
    case mainDirections of
        [ RIGHT, DOWN, LEFT ] ->
            "J"

        [ RIGHT, DOWN, LEFT, UP ] ->
            "J"

        [ RIGHT, DOWN, RIGHT ] ->
            "Z"

        [ RIGHT, LEFT, RIGHT ] ->
            "Z"

        [ RIGHT, DOWN, LEFT, DOWN, RIGHT ] ->
            "Z"

        [ RIGHT, DOWN, LEFT, RIGHT ] ->
            -- TODO: Check if it is "2"
            "Z"

        [ RIGHT, DOWN, LEFT, RIGHT, DOWN, LEFT ] ->
            "3"

        [ RIGHT, DOWN ] ->
            "7"

        _ ->
            "Unknown"


recognizeSymbolLeft : Symbol -> String
recognizeSymbolLeft symbol =
    let
        mainDirections = symbol.mainDirections
    in
    case mainDirections of
        [ LEFT, DOWN ] ->
            "F"

        [ LEFT, DOWN, RIGHT, LEFT, DOWN, RIGHT ] ->
            "E"

        [ LEFT, DOWN, RIGHT, UP, LEFT ] ->
            -- TODO: Check if is "O"
            "G"

        [ LEFT, DOWN, RIGHT, DOWN, LEFT ] ->
            -- TODO: Check if it is "5"
            "S"

        [ LEFT, DOWN, RIGHT, UP, LEFT, DOWN ] ->
            "6"

        [ LEFT, DOWN, RIGHT, DOWN, LEFT, UP, RIGHT, UP, LEFT ] ->
            "8"

        [ LEFT, DOWN, RIGHT, DOWN, LEFT, UP, LEFT ] ->
            "8"

        [ LEFT, DOWN, RIGHT, DOWN, LEFT, UP, RIGHT, UP ] ->
            "8"

        [ LEFT, DOWN, RIGHT, DOWN, LEFT, UP ] ->
            "8"

        [ LEFT, DOWN, RIGHT, UP, DOWN, LEFT ] ->
            "9"

        _ ->
            "Unknown"



sym_A_N : Symbol -> String
sym_A_N symbol =
    let
        quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
    in
        case quadrantStartToEnd of
            (III, III) ->
                "A"

            (III, I) ->
                "N"

            (III, II) ->
                "A"

            _ ->
                "Not recognized"


sym_A_2_3 : Symbol -> String
sym_A_2_3 symbol =
    let
        (startQuadrant, endQuadrant) = (symbol.startQuadrant, symbol.endQuadrant)
    in
        case (startQuadrant, endQuadrant) of
            (III, III) ->
                "A"

            (II, IV) ->
                "2"

            (II, III) ->
                "3"

            _ ->
                "Not recognized"



sym_N_M_R_3 : Symbol -> String
sym_N_M_R_3 symbol =
    let
        (startQuadrant, endQuadrant) = (symbol.startQuadrant, symbol.endQuadrant)
    in
        case (startQuadrant, endQuadrant) of
            (III, I) ->
                "N"

            -- "M" or "R"
            (III, IV) ->
                case Array.length symbol.corners of
                    2 ->
                        "R"

                    3 ->
                        "M"

                    _ -> "Not recognized"


            (II, III) ->
                "3"

            _ ->
                "Not recognized"