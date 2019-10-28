module Main exposing (Point, main)

import Array exposing (fromList)
import Browser
import Element as Element exposing (fill, rgb)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, on, onMouseDown, onMouseUp)
import Json.Decode as Json exposing (..)
import String exposing (fromInt)
import Svg exposing (mpath, svg)
import Svg.Attributes exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


cWidth =
    500


cHeight =
    500



-- MODEL


type alias Model =
    { path : List Point
    , smoothedPath : List Point
    , thinnedPath : List Point
    , curvedPath : List Point
    , currentlyDrawing : Bool
    , pointerPosition : Position
    , smoothingFactor : Float
    , thinningFactor : Int
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { path = []
      , smoothedPath = []
      , thinnedPath = []
      , curvedPath = []
      , currentlyDrawing = False
      , pointerPosition = Position 0 0
      , smoothingFactor = 0.75
      , thinningFactor = 10
      }
    , Cmd.none
    )


type alias Point =
    ( Int, Int )


type alias Position =
    { x : Int
    , y : Int
    }


pointToString : ( Int, Int ) -> String
pointToString point =
    let
        x =
            String.fromInt <| Tuple.first point

        y =
            String.fromInt <| Tuple.second point
    in
    x ++ "," ++ y



-- UPDATE


type Msg
    = DrawStart
    | DrawEnd
    | UpdatePointerPosition Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePointerPosition pos ->
            case model.currentlyDrawing of
                True ->
                    let
                        pathSoFar =
                            model.path

                        newPoint =
                            ( pos.x, pos.y )

                        pathNew =
                            newPoint :: pathSoFar

                        smoothPath =
                            smoothing
                                pathNew
                                model.smoothedPath
                                model.smoothingFactor
                                newPoint

                        thinnedPath =
                            thinning
                                smoothPath
                                model.thinnedPath
                                model.thinningFactor


                    in
                    ( { model
                        | pointerPosition = pos
                        , path = pathNew
                        , smoothedPath = smoothPath
                        , thinnedPath = thinnedPath
                      }
                    , Cmd.none
                    )

                False ->
                    ( { model | pointerPosition = pos }, Cmd.none )

        DrawStart ->
            ( { model
                | currentlyDrawing = True
                , path = []
                , smoothedPath = []
                , thinnedPath = []
              }
            , Cmd.none
            )

        DrawEnd ->
            ( { model | currentlyDrawing = False }, Cmd.none )


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


fromMaybe : Maybe Point -> Point
fromMaybe x =
    case x of
        Just y ->
            y

        Nothing ->
            ( 0, 0 )


thinning : List Point -> List Point -> Int -> List Point
thinning smoothedPath thinnedPath tf =
    if (List.length thinnedPath > 1) then
        let
            -- 1. Take last smoothed Point
            ( xSi, ySi ) =
                fromMaybe <| List.head smoothedPath
            -- 2. Take last thinned Point
            ( xTj_sub_1, yTj_sub_1 ) =
                fromMaybe <| List.head thinnedPath

        in
        -- 3. Calculate if the difference between last smoothed
        -- and last thinned is larger than the thinningFactor (tf)
        if ((abs (xSi - xTj_sub_1)) >= tf)
           || ((abs (ySi - yTj_sub_1)) >= tf)
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



-- VIEW


view : Model -> Html Msg
view model =
    let
        mouseX =
            String.fromInt model.pointerPosition.x

        mouseY =
            String.fromInt model.pointerPosition.y

        vBox =
            viewBox <| "0 0 " ++ String.fromFloat cWidth ++ " " ++ String.fromFloat cHeight

        linesToDraw =
            pathToSvg model.path

        smoothedLines =
            pathToSvg model.smoothedPath

        thinnedLines =
            pathToSvg model.thinnedPath

    in
    Element.layout [] <|
        Element.column
            [ Background.color (rgb 253 246 227)
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 0
            ]
            [ Element.text <|
                "Number of Points : "
                    ++ (String.fromInt <| List.length model.path)
            , Element.text <|
                "Current Coordinates: "
                    ++ "x: "
                    ++ mouseX
                    ++ "y: "
                    ++ mouseY
            , drawingBox vBox mouseX mouseY linesToDraw
            , drawingBox vBox mouseX mouseY smoothedLines
            , drawingBox vBox mouseX mouseY thinnedLines
            --        , mouseCircle mouseX mouseY
            ]


pathToSvg : List Point -> Svg.Svg msg
pathToSvg points =
    let
        x =
            List.map pointToString points
                |> String.join " "

        polyColor =
            "grey"
    in
    Svg.polyline
        [ Svg.Attributes.points x
        , Svg.Attributes.fill "none"
        , stroke polyColor
        , strokeWidth "3"
        ]
        []


drawingBox : Svg.Attribute Msg -> String -> String -> Svg.Svg Msg -> Element.Element Msg
drawingBox vBox mouseX mouseY linesToDraw =
    Element.html <|
        Svg.svg
            [ vBox
            , Svg.Attributes.width <| String.fromInt cWidth ++ "px"
            , Svg.Attributes.width <| String.fromInt cHeight ++ "px"
            , Html.Attributes.style "border" "5px solid grey"
            , Html.Attributes.style "border-radius" "5px"

            --        , Html.Events.on "mousemove" (Json.map UpdatePointerPosition offsetPosition)
            , Html.Events.stopPropagationOn "mousemove" offsetPosition
            , Html.Events.onMouseDown DrawStart
            , Html.Events.onMouseUp DrawEnd
            ]
            [ Svg.rect
                [ Svg.Attributes.width <| String.fromInt cWidth
                , Svg.Attributes.height <| String.fromInt cHeight
                , Svg.Attributes.fill "#fdf6e3"
                , Svg.Attributes.x "0"
                , Svg.Attributes.y "0"
                ]
                []
            , Svg.circle
                [ cx mouseX
                , cy mouseY
                , r "10"
                , Svg.Attributes.fill "#fd635e"
                , Svg.Attributes.opacity "0.7"
                ]
                []
            , linesToDraw
            ]


offsetPosition : Json.Decoder ( Msg, Bool )
offsetPosition =
    map2 Position
        (field "offsetX" int)
        (field "offsetY" int)
        |> Json.map UpdatePointerPosition
        |> Json.map (\msg -> ( msg, True ))



--eventConfig : Bool -> Bool -> msg -> {message: Decoder Msg , stopPropagation : Bool, preventDefault : Bool}
--eventConfig stopPropagation preventDefault msg =
--    { message = options
--    , stopPropagation = stopPropagation
--    , preventDefault = preventDefault
--    }
--options = Json.map UpdatePointerPosition offsetPosition
--decoder = Json.map UpdatePointerPosition offsetPosition
--mouseMoveDecoder : Position -> Json.Decoder (Msg, Bool)
--mouseMoveDecoder pos =
--     let
--         options message =
--             { message = message
--             , stopPropagation = True
--             , preventDefault = False
--             }
--
--         decoder =
--             offsetPosition
--     in
--     Html.Events.custom "mousemove" decoder
--
--
