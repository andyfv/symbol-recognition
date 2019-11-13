module Symbols exposing (recognizeSymbolDOWN, recognizeSymbolLEFT, recognizeSymbolRIGHT, recognizeSymbolUP)

import Array exposing (toList)
import Types exposing (..)


--symbolQuadrants =
--    { a = (III, III)
--    , b =
--    }


-- UP


recognizeSymbolUP : Symbol -> String
recognizeSymbolUP symbol =
    let
        mainDirections = symbol.mainDirections

    in
    case mainDirections of
        [ UP, DOWN, EMPTY, EMPTY ] -> rec_UDEE symbol

        [ UP, DOWN, UP, EMPTY ] -> rec_UDUE symbol

        [ UP, RIGHT, DOWN, UP ] -> rec_URDU symbol

        [ UP, DOWN, UP, DOWN ] -> rec_UDUD symbol

        [ UP, RIGHT, DOWN, RIGHT] -> rec_URDR symbol

        [ UP, DOWN, RIGHT, UP] -> rec_UDRU symbol

        [ UP, RIGHT, DOWN, LEFT] -> rec_URDL symbol

        [ UP, LEFT, DOWN, RIGHT] -> rec_ULDR symbol

        [ UP, RIGHT, DOWN, EMPTY] -> rec_UPDE symbol

        [ UP, DOWN, UP, LEFT] -> "A"

        _ ->
            "Unknown"

-- #Symbols: A, M
rec_UDUD : Symbol -> String
rec_UDUD symbol =
    let
        quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
    in
        case quadrantStartToEnd of
            (III, IV) -> "M"
            (III, III) -> "A"
            (III, II) -> "A"
            _ -> "Not Recognized"


-- #Symbols: A, 7
rec_UPDE : Symbol -> String
rec_UPDE symbol =
    let
        quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
    in
        case quadrantStartToEnd of
            (II, IV) -> "7"
            (II, III) -> "7"
            (III, III) -> "A"
            (III, II) -> "A"
            _ -> "Not Recognized"


-- #Symbols: A, 1
rec_UDEE : Symbol -> String
rec_UDEE symbol =
    let
        quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirection = symbol.secondaryDirections
    in
        case quadrantStartToEnd of
            (II, IV) -> "1"
            (III, IV) -> "1"
            (III, III) -> "A"
            (III, II) -> "A"
            _ -> "Not Recognized"

-- #Symbols: C, E, G, S, Q
rec_ULDR : Symbol -> String
rec_ULDR symbol =
    let
        quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirection = symbol.secondaryDirections
    in
        case secondaryDirection of
            [EMPTY, EMPTY] -> "C"
            [LEFT, DOWN] -> "E"
            [UP, LEFT] -> -- TODO: Since "G" can be confused with "E", redo cornerPoints to be Quadrants
                case quadrantStartToEnd of
                    (I, IV) -> "E"
                    (I, III) -> "6"
                    (II, III) -> "6"
                    _ -> "Not Recognized"
            [DOWN, LEFT] -> "S"
            [UP, EMPTY] -> "G"
            [DOWN, RIGHT] -> "E"
            [UP, RIGHT] ->  -- TODO: Since "G" can be confused with "E, Q", redo cornerPoints to be Quadrants
                case quadrantStartToEnd of
                    (I, IV) -> "E"
                    (II, IV) -> "E"
                    (IV, IV) -> "Q"
                    (III, IV) -> "Q"
                    _ -> "Not Recognized"
            [UP, DOWN] ->
                case quadrantStartToEnd of
                    (IV, IV) -> "Q"
                    (III, IV) -> "Q"
                    (I, IV) -> "E"
                    _ -> "Not Recognized"

            _ -> "Not Recognized"

-- #Symbols: B, J, P, R, 2, 3
rec_URDL : Symbol -> String
rec_URDL symbol =
    let
        quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirection = symbol.secondaryDirections
    in
        case secondaryDirection of
            [RIGHT, DOWN] ->
                case quadrantStartToEnd of
                    (III, III) -> "B"
                    (II, III) -> "3"
                    (III, IV) -> "R"
                    _ -> "Not Recognized"

            [EMPTY, EMPTY] ->
                case quadrantStartToEnd of
                    (II, III) -> "J"
                    (III, III) -> "P"
                    (III, II) -> "P"
                    _ -> "Not Recognized"

            [UP, RIGHT] ->
                case quadrantStartToEnd of
                    (II, IV) -> "2"
                    (II, III) -> "3"
                    (III, IV) -> "R"
                    (III, III) -> "B"
                    _ -> "Not Recognized"

            [RIGHT, EMPTY] ->
                case quadrantStartToEnd of
                    (II, IV) -> "2"
                    (III, IV) -> "R"
                    _ -> "Not Recognized"

            [DOWN, RIGHT] ->
                case quadrantStartToEnd of
                    (II, IV) -> "2"
                    (II, III) -> "3"
                    (III, IV) -> "R"
                    (III, III) -> "B"
                    _ -> "Not Recognized"

            [RIGHT, UP] -> "B"

            _ ->
                "Not recognized"

-- #Symbols: A, N
rec_UDUE : Symbol -> String
rec_UDUE symbol =
    let
        quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
    in
        case quadrantStartToEnd of
            (III, III) -> "A"
            (III, I) -> "N"
            (III, II) -> "A"
            _ -> "Not recognized"

-- #Symbols: A, N, R, 2, 3,
rec_URDU : Symbol -> String
rec_URDU symbol =
    let
        (startQuadrant, endQuadrant) = (symbol.startQuadrant, symbol.endQuadrant)
    in
        case (startQuadrant, endQuadrant) of
            (III, III) -> "A"
            (II, IV) -> "2"
            (II, III) -> "3"
            (III, I) -> "N"
            (III, IV) -> "R"
            _ -> "Not recognized"


-- #Symbols: B, N, M, R, 2, 3
rec_URDR : Symbol -> String
rec_URDR symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
    in
        case secondaryDirections of
            [EMPTY, EMPTY] ->
                case quadrantStartToEnd of
                    (III, I) -> "N"
                    (II, IV) -> "2"
                    (III, IV) -> "R"
                    _ -> "Not recognized"

            [UP, EMPTY] -> "N"
            [UP, RIGHT] ->
                case quadrantStartToEnd of
                    (III, IV) -> "M"
                    (III, III) -> "B"
                    _ -> "Not recognized"
            [DOWN, EMPTY] -> "R"
            [DOWN, LEFT] ->
                case quadrantStartToEnd of
                    (III, III) -> "B"
                    (II, III) -> "3"
                    _ -> "Not recognized"

            _ -> "Not recognized"


-- #Symbols: A, N, M
rec_UDRU : Symbol -> String
rec_UDRU symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
    in
        case secondaryDirections of
            [EMPTY, EMPTY] ->
                case quadrantStartToEnd of
                    (III, III) -> "A"
                    (III, II) -> "A"
                    (III, I) -> "N"
                    _ -> "Not recognized"
            [DOWN, EMPTY] -> "M"
            [RIGHT, DOWN] -> "M"
            [LEFT, EMPTY] -> "A"
            _ -> "Not recognized"




-- DOWN


recognizeSymbolDOWN : Symbol -> String
recognizeSymbolDOWN symbol =
    let
        mainDirections = symbol.mainDirections
    in
    case mainDirections of
        [ DOWN, EMPTY, EMPTY, EMPTY ] -> "I"

        [ DOWN, UP, EMPTY, EMPTY ] -> "V"

        [ DOWN, RIGHT, UP, EMPTY ] ->  rec_DRUE symbol

        [ DOWN, RIGHT, EMPTY, EMPTY ] -> "L"

        [ DOWN, RIGHT, UP, LEFT ] -> rec_DRUL symbol

        [ DOWN, RIGHT, UP, DOWN ] -> rec_DRUD symbol

        [ DOWN, RIGHT, UP, RIGHT ] -> rec_DRUR symbol

        [ DOWN, UP, RIGHT, UP ] -> rec_DURU symbol

        [ DOWN, UP, DOWN, RIGHT] -> rec_DUDR symbol

        [ DOWN, UP, RIGHT, DOWN] -> rec_DURD symbol

        [ DOWN, UP, LEFT, RIGHT] -> "K"

        [ DOWN, LEFT, UP, EMPTY] -> "X"

        [ DOWN, RIGHT, DOWN, LEFT] -> "X"

        [ DOWN, LEFT, UP, RIGHT] -> "X"

        [ DOWN, LEFT, RIGHT, UP] -> "X"

        [ DOWN, RIGHT, LEFT, UP] -> "X"

        _ ->
            "Unknown"


-- #Symbols: D, X, 4
rec_DRUL : Symbol -> String
rec_DRUL symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
    in
        case secondaryDirections of
            [DOWN, EMPTY] -> "4"
            [EMPTY, EMPTY] ->
                case quadrantStartToEnd of
                    (II, II) -> "D"
                    (II, I) -> "X"
                    (I, II) -> "X"
                    _ -> "Not recognized"

            _ -> "Not recognized"



-- #Symbols: K
rec_DUDR : Symbol -> String
rec_DUDR symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
    in
        case secondaryDirections of
            [ DOWN, EMPTY ] -> "K"
            [ UP, EMPTY ] -> "K"
            _ -> "Not recognized"


-- #Symbols: K, W
rec_DURD : Symbol -> String
rec_DURD symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
    in
        case secondaryDirections of
            [ RIGHT, UP ] -> "W"
            [ DOWN, RIGHT ] -> "K"
            _ -> "Not recognized"


-- #Symbols: H, K
rec_DURU : Symbol -> String
rec_DURU symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
        numberOfCorners = Array.length symbol.corners
    in
        case secondaryDirections of
            [ RIGHT, UP ] ->
                case numberOfCorners of
                    3 -> "K"
                    4 -> "H"
                    _ -> "Not recognized"

            [ DOWN, UP ] -> "K"
            [ RIGHT, DOWN ] -> "K"
            [ DOWN, RIGHT ] -> "K"
            [ DOWN, EMPTY ] -> "H"
            [ LEFT, DOWN ] -> "H"
            _ -> "Not recognized"



-- #Symbols: K, U, W, 4
rec_DRUD : Symbol -> String
rec_DRUD symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
        numberOfCorners = Array.length symbol.corners
    in
        case secondaryDirections of
            [ EMPTY, EMPTY ] ->
                case numberOfCorners of
                    1 -> "U"
                    2 -> "4"
                    3 -> "4"
                    _ -> "Not recognized"

            [ LEFT, EMPTY] -> "Y"
            [ LEFT, UP ] -> "Y"
            [ RIGHT, DOWN ] -> "K"
            [ RIGHT, UP ] -> "W"
            [ UP, EMPTY ] -> "W"
            _ -> "Not recognized"



-- #Symbols: D, H, K, Y, W, X, 4
rec_DRUR : Symbol -> String
rec_DRUR symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
    in
        case secondaryDirections of
            [UP, RIGHT] ->
                case quadrantStartToEnd of
                    (II, III) -> "Y"
                    (II, II) -> "D"
                    _ -> "Not recognized"

            [RIGHT, DOWN] -> "H"
            [DOWN, LEFT] -> "Y"
            [UP, LEFT] -> "D"
            [UP, EMPTY] -> "X"
            [UP, DOWN] ->
                case quadrantStartToEnd of
                    (II, I) -> "W"
                    (II, IV) -> "4"
                    (I, IV) -> "4"
                    _ -> "Not recognized"

            [DOWN, UP] -> "W"
            [DOWN, RIGHT] ->
                case quadrantStartToEnd of
                    (II, I) -> "W"
                    (II, IV) -> "K"
                    _ -> "Not recognized"

            _ -> "Not recognized"



-- #Symbols: D, U, V
rec_DRUE : Symbol -> String
rec_DRUE symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
        numberOfCorners = Array.length symbol.corners
    in
        case quadrantStartToEnd of
            (II, II) -> "D"
            (III, IV) -> "U"
            (II, I) -> "V"
            _ -> "Not recognized"




-- RIGHT


recognizeSymbolRIGHT : Symbol -> String
recognizeSymbolRIGHT symbol =
    let
        mainDirections = symbol.mainDirections
    in
    case mainDirections of
        [ RIGHT, DOWN, LEFT, EMPTY ] -> "J"

        [ RIGHT, DOWN, LEFT, DOWN ] -> rec_RDLD symbol

        [ RIGHT, DOWN, LEFT, RIGHT] -> rec_RDLR symbol

        [ RIGHT, DOWN, RIGHT, DOWN] -> "3"

        [ RIGHT, UP, RIGHT, DOWN ] -> rec_RURD symbol

        [ RIGHT, DOWN, EMPTY, EMPTY ] -> "7"

        [ RIGHT, DOWN, RIGHT, EMPTY] -> "2"

        [ RIGHT, UP, LEFT, DOWN] -> "Q"

        _ ->
            "Unknown"



-- #Symbols: 2, 3
rec_RURD : Symbol -> String
rec_RURD symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
        numberOfCorners = Array.length symbol.corners
    in
        case secondaryDirections of
            [RIGHT, DOWN] -> "3"
            [LEFT, DOWN] -> "2"
            _ -> "Not recognized"


-- #Symbols: Z, 2, 3
rec_RDLR : Symbol -> String
rec_RDLR symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
        numberOfCorners = Array.length symbol.corners
    in
        case secondaryDirections of
            [EMPTY, EMPTY] ->
                case numberOfCorners of
                    2 -> "Z"
                    1 -> "2"
                    _ -> "Not recognized"
            [DOWN, LEFT] -> "3"
            _ -> "Not recognized"



-- #Symbols: Z, 3
rec_RDLD : Symbol -> String
rec_RDLD symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
        numberOfCorners = Array.length symbol.corners
    in
        case secondaryDirections of
            [RIGHT, EMPTY] -> "Z"
            [RIGHT, DOWN] -> "3"
            _ -> "Not recognized"




-- LEFT


recognizeSymbolLEFT : Symbol -> String
recognizeSymbolLEFT symbol =
    let
        mainDirections = symbol.mainDirections
    in
    case mainDirections of
        [ LEFT, DOWN, EMPTY, EMPTY ] -> "F"

        [ LEFT, DOWN, RIGHT, LEFT] -> "E"

        [ LEFT, DOWN, LEFT, EMPTY] -> "S"

        [ LEFT, DOWN, RIGHT, DOWN] -> rec_LDRD symbol

        [ LEFT, DOWN, RIGHT, UP] -> rec_LDRU symbol

        [ LEFT, DOWN, UP, RIGHT] -> "5"

        [ LEFT, RIGHT, DOWN, EMPTY] -> "T"

        _ ->
            "Unknown"



-- #Symbols: E, S, T, 5, 8
rec_LDRD : Symbol -> String
rec_LDRD symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
        numberOfCorners = Array.length symbol.corners
    in
        case secondaryDirections of
            [LEFT, UP] -> "8"
            [EMPTY, EMPTY] -> "T"
            [RIGHT, EMPTY] ->
                case quadrantStartToEnd of
                    (I, IV) -> "E"
                    (I, III) -> "S"
                    _ -> "Not recognized"

            [LEFT, EMPTY] ->
                case numberOfCorners of
                    0 -> "S"
                    1 -> "5"
                    2 -> "5"
                    _ -> "Not recognized"
            _ -> "Not recognized"



-- #Symbols: E, G, O, 5, 6, 9
rec_LDRU : Symbol -> String
rec_LDRU symbol =
    let quadrantStartToEnd = (symbol.startQuadrant, symbol.endQuadrant)
        secondaryDirections = symbol.secondaryDirections
        numberOfCorners = Array.length symbol.corners
    in
        case secondaryDirections of
            [LEFT, DOWN] ->
                case quadrantStartToEnd of
                    (I, IV) -> "E"
                    (I, III) -> "6"
                    (II, III) -> "6"
                    _ -> "Not recognized"
            [RIGHT, UP] -> "5"
            [RIGHT, DOWN] -> "5"
            [DOWN, LEFT] -> "9"
            [LEFT, EMPTY] ->
                case quadrantStartToEnd of
                    (I, I) -> "O"
                    (I, II) -> "O"
                    (II, II) -> "O"
                    (II, I) -> "O"
                    (I, III) -> "G"
                    (I, IV) -> "G"
                    _ -> "Not recognized"
            _ -> "Not recognized"