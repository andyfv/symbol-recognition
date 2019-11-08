module Main exposing (main)

import Array exposing (Array, fromList)
import Browser
import DataManipulation exposing (..)
import Types exposing (..)
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


canvasSize =
    { width = 300
    , height = 400
    }



-- MODEL


type alias Model =
    { path : Array Point
    , smoothedPath : Array Point
    , thinnedPath : Array Point
    , curvePath : Array Direction
    , corners : Array Point
    , currentlyDrawing : Bool
    , pointerPosition : Position
    , smoothingFactor : Float
    , thinningFactor : Int
    , cornerThreshold : Float
    , startingCoordinates : Point
    , endingCoordinates : Point
    , recognizedSymbol : String
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { path = Array.empty
      , smoothedPath = Array.empty
      , thinnedPath = Array.empty
      , curvePath = Array.empty
      , corners = Array.empty
      , currentlyDrawing = False
      , pointerPosition = Position 0 0
      , smoothingFactor = 0.75
      , thinningFactor = 3
      , cornerThreshold = 110
      , startingCoordinates = (0, 0)
      , endingCoordinates = (0, 0)
      , recognizedSymbol = ""
      }
    , Cmd.none
    )


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
                        rawPointsSoFar =
                            model.path

                        newPoint =
                            ( pos.x, pos.y )

                        rawPointsNew =
                            Array.push newPoint rawPointsSoFar

                        smoothedPath =
                            smoothing
                                rawPointsNew
                                model.smoothedPath
                                model.smoothingFactor
                                newPoint

                        thinnedPath =
                            thinning
                                smoothedPath
                                model.thinnedPath
                                model.thinningFactor

                        curvaturePath =
                            getDirection thinnedPath model.curvePath

                        corners =
                            detectCorners
                            thinnedPath
                            model.corners
                            model.cornerThreshold


                    in
                    ( { model
                        | pointerPosition = pos
                        , path = rawPointsNew
                        , smoothedPath = smoothedPath
                        , thinnedPath = thinnedPath
                        , curvePath = curvaturePath
                        , corners = corners
                      }
                    , Cmd.none
                    )

                False ->
                    ( { model | pointerPosition = pos }, Cmd.none )

        DrawStart ->
            ( { model
                | currentlyDrawing = True
                , path = Array.empty
                , smoothedPath = Array.empty
                , thinnedPath = Array.empty
                , curvePath = Array.empty
                , corners = Array.empty
              }
            , Cmd.none
            )

        DrawEnd ->
            let
                (startQuadrant, endQuadrant) = getStartAndEndPosition model.thinnedPath

                symbol : Symbol
                symbol =
                    { directions = model.curvePath
                    , corners = model.corners
                    , startQuadrant = startQuadrant
                    , endQuadrant = endQuadrant
                    }

                recognizedSymbol = recognizeSymbol symbol
            in
            ( { model
                | currentlyDrawing = False
                , recognizedSymbol = recognizedSymbol
                }
            , Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        mouseX =
            String.fromInt model.pointerPosition.x

        mouseY =
            String.fromInt model.pointerPosition.y

        vBox =
            viewBox <|
                "0 0 "
                    ++ String.fromFloat canvasSize.width
                    ++ " "
                    ++ String.fromFloat canvasSize.height

        linesToDraw =
            pathToSvg model.path

        smoothedLines =
            pathToSvg model.smoothedPath

        thinnedLines =
            pathToSvg model.thinnedPath
    in
    Debug.log (model.recognizedSymbol)
--      Debug.log ("startCoord: " ++ Debug.toString model.startingCoordinates)
--      Debug.log ("endCoord: " ++ Debug.toString model.endingCoordinates)
--    Debug.log (Debug.toString model.corners)
    Debug.log (Debug.toString model.curvePath)
--    <|
        Element.layout
            []
        <|
            Element.column
                [ Background.color (rgb 253 246 227)
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 0
                ]
                [ Element.text <|
                    "Number of Points : "
                        ++ (String.fromInt <| Array.length model.path)
                , Element.text <|
                    "Current Coordinates: "
                        ++ "x: "
                        ++ mouseX
                        ++ "y: "
                        ++ mouseY
                , Element.row []
                    [ drawingBox vBox mouseX mouseY linesToDraw
                    , drawingBox vBox mouseX mouseY smoothedLines
                    , drawingBox vBox mouseX mouseY thinnedLines
                    ]

                --        , mouseCircle mouseX mouseY
                ]


pathToSvg : Array Point -> Svg.Svg msg
pathToSvg points =
    let
        x =
            List.map pointToString (Array.toList points)
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
            , Svg.Attributes.width <| String.fromInt canvasSize.width ++ "px"
            , Svg.Attributes.height <| String.fromInt canvasSize.height ++ "px"
            , Html.Attributes.style "border" "5px solid grey"
            , Html.Attributes.style "border-radius" "5px"

            -- , Html.Events.on "mousemove" (Json.map UpdatePointerPosition offsetPosition)
            -- , Html.Events.on "mousedown" (Json.map DrawStart mouseCoord)
            -- , Html.Events.on "mouseup" (Json.map DrawEnd mouseCoord)
            , Html.Events.onMouseDown DrawStart
            , Html.Events.onMouseUp DrawEnd
            , Html.Events.stopPropagationOn "mousemove" offsetPosition
            ]
            [ Svg.rect
                [ Svg.Attributes.width <| String.fromInt canvasSize.width
                , Svg.Attributes.height <| String.fromInt canvasSize.height
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
    mouseCoord
        |> Json.map UpdatePointerPosition
        |> Json.map (\msg -> ( msg, True ))


mouseCoord :  Decoder Position
mouseCoord =
    map2 Position
        (field "offsetX" int)
        (field "offsetY" int)





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
